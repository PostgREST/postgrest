{-# LANGUAGE FlexibleContexts #-}
module App (app, sqlError, isSqlError) where

import Control.Monad (join)
import Control.Arrow ((***))
import Control.Applicative

import Data.Text hiding (map)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA ((=~))
import Data.Ord (comparing)
import Data.Ranged.Ranges (emptyRange)
import Data.HashMap.Strict (keys, elems, filterWithKey, toList)
import Data.String.Conversions (cs)
import Data.List (sortBy)
import Data.Functor.Identity
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.HTTP.Base (urlEncodeVars)
import Network.Wai

import Data.Aeson
import Data.Monoid
import qualified Hasql as H
import qualified Hasql.Postgres as H

import Auth
import PgQuery
import RangeQuery
import PgStructure
import PgError
import Text.Parsec hiding (Column)

app :: BL.ByteString -> Request -> H.Tx H.Postgres s Response
app reqBody req =
  case (path, verb) of
    ([], _) -> do
      body <- encode <$> tables (cs schema)
      return $ responseLBS status200 [jsonH] $ cs body

    ([table], "OPTIONS") -> do
      let t = QualifiedTable schema (cs table)
      cols <- columns t
      pkey <- map cs <$> primaryKeyColumns t
      return $ responseLBS status200 [jsonH, allOrigins]
        $ encode (TableOptions cols pkey)

    ([table], "GET") ->
      if range == Just emptyRange
      then return $ responseLBS status416 [] "HTTP Range error"
      else do
        let qt = QualifiedTable schema (cs table)
        let select = coerce $
              ("select ",[],mempty) <>
                  parentheticT (
                    whereT qq $ countRows qt
                  ) <> commaq <> (
                  asJsonWithCount
                  . limitT range
                  . orderT (orderParse qq)
                  . whereT qq
                  $ selectStar qt
                )
        row <- H.single select
        let (tableTotal, queryTotal, body) =
              fromMaybe (0, 0, Just "" :: Maybe Text) row
            from = fromMaybe 0 $ rangeOffset <$> range
            to = from+queryTotal-1
            contentRange = contentRangeH from to tableTotal
            status = rangeStatus from to tableTotal
            canonical = urlEncodeVars
                          . sortBy (comparing fst)
                          . map (join (***) cs)
                          . parseSimpleQuery
                          $ rawQueryString req
        return $ responseLBS status
          [jsonH, contentRange,
            ("Content-Location",
             "/" <> cs table <>
                if Prelude.null canonical then "" else "?" <> cs canonical
            )
          ] (cs $ fromMaybe "[]" body)

    (["postgrest", "users"], "POST") -> do
      let user = decode reqBody :: Maybe AuthUser

      case user of
        Nothing -> return $ responseLBS status400 [jsonH] $
          encode . object $ [("message", String "Failed to parse user.")]
        Just u -> do
          _ <- addUser (cs $ userId u)
            (cs $ userPass u) (cs $ userRole u)
          return $ responseLBS status201
            [ jsonH
            , (hLocation, "/postgrest/users?id=eq." <> cs (userId u))
            ] ""

    ([table], "POST") ->
      handleJsonObj reqBody $ \obj -> do
        let qt = QualifiedTable schema (cs table)
            query = coerce $
              insertInto qt (map cs $ keys obj) (elems obj)
        row <- H.single query
        let (Identity insertedJson) = fromMaybe (Identity "{}" :: Identity Text) row
            Just inserted = decode (cs insertedJson) :: Maybe Object

        primaryKeys <- map cs <$> primaryKeyColumns qt
        let primaries = if Prelude.null primaryKeys
            then inserted
            else filterWithKey (const . (`elem` primaryKeys)) inserted
        let params = urlEncodeVars
              $ map (\t -> (cs $ fst t, "eq." <> cs (unquoted $ snd t)))
              $ sortBy (comparing fst) $ toList primaries
        return $ responseLBS status201
          [ jsonH
          , (hLocation, "/" <> cs table <> "?" <> cs params)
          ] ""

    ([table], "PUT") ->
      handleJsonObj reqBody $ \obj -> do
        let qt = QualifiedTable schema (cs table)
        primaryKeys <- primaryKeyColumns qt
        let specifiedKeys = map (cs . fst) qq
        if S.fromList primaryKeys /= S.fromList specifiedKeys
          then return $ responseLBS status405 []
               "You must speficy all and only primary keys as params"
          else do
            tableCols <- map (cs . colName) <$> columns qt
            let cols = map cs $ keys obj
            if S.fromList tableCols == S.fromList cols
              then do
                let vals = elems obj
                H.unit . coerce $ iffNotT
                        (whereT qq $ update qt cols vals)
                        (insertSelect qt cols vals)
                return $ responseLBS status204 [ jsonH ] ""

              else return $ if Prelude.null tableCols
                then responseLBS status404 [] ""
                else responseLBS status400 []
                   "You must specify all columns in PUT request"

    ([table], "PATCH") ->
      handleJsonObj reqBody $ \obj -> do
        let qt = QualifiedTable schema (cs table)
        H.unit
          $ coerce
          $ whereT qq
          $ update qt (map cs $ keys obj) (elems obj)
        return $ responseLBS status204 [ jsonH ] ""

    ([table], "DELETE") -> do
      let qt = QualifiedTable schema (cs table)
      let del = coerce $ countT
            . returningStarT
            . whereT qq
            $ deleteFrom qt
      row <- H.single del
      let (Identity deletedCount) = fromMaybe (Identity 0 :: Identity Int) row
      return $ if deletedCount == 0
         then responseLBS status404 [] ""
         else responseLBS status204 [("Content-Range", "*/"<> cs (show deletedCount))] ""

    (_, _) ->
      return $ responseLBS status404 [] ""

  where
    path   = pathInfo req
    verb   = requestMethod req
    qq     = queryString req
    hdrs   = requestHeaders req
    schema = requestedSchema hdrs
    range  = rangeRequested hdrs
    allOrigins = ("Access-Control-Allow-Origin", "*") :: Header
    coerce (q, args, All b) = (q, args, b)


isSqlError :: H.Error -> Maybe H.Error
isSqlError = Just

sqlError :: H.Error -> Response
sqlError err =
  let inside = case err of
        H.CantConnect _ ->
          "Message: \"Cannot connect to postgres server\""
        H.ConnectionLost t -> t
        H.ErroneousResult t -> t
        H.UnexpectedResult t -> t
        H.UnparsableTemplate t -> t
        H.UnparsableRow t -> t
        H.NotInTransaction -> "An operation which requires a"
          <> "database transaction was executed without one" in
  either
    (\hint ->
      responseLBS status500
      [(hContentType, "application/json")]
      (cs . encode . object $ [
          ("message", String $
            "Failed to parse exception:" <> inside)
        , ("hint", String . cs . show $ hint)]))
    (\msg ->
      responseLBS (httpStatus msg)
      [(hContentType, "application/json")]
      (encode msg))
    (parse message "" inside)


rangeStatus :: Int -> Int -> Int -> Status
rangeStatus from to total
  | from > total            = status416
  | (1 + to - from) < total = status206
  | otherwise               = status200

contentRangeH :: Int -> Int -> Int -> Header
contentRangeH from to total =
  ("Content-Range",
    if total == 0 || from > total
    then "*/" <> cs (show total)
    else cs (show from)  <> "-"
       <> cs (show to)    <> "/"
       <> cs (show total)
  )

requestedSchema :: RequestHeaders -> Text
requestedSchema hdrs =
  case verStr of
       Just [[_, ver]] -> ver
       _ -> "1"

  where verRegex = "version[ ]*=[ ]*([0-9]+)" :: String
        accept = cs <$> lookup hAccept hdrs :: Maybe Text
        verStr = (=~ verRegex) <$> accept :: Maybe [[Text]]

jsonH :: Header
jsonH = (hContentType, "application/json")

handleJsonObj :: BL.ByteString -> (Object -> H.Tx H.Postgres s Response)
              -> H.Tx H.Postgres s Response
handleJsonObj reqBody handler = do
  let p = eitherDecode reqBody
  case p of
    Left err ->
      return $ responseLBS status400 [jsonH] jErr
      where
        jErr = encode . object $
          [("message", String $ "Failed to parse JSON payload. " <> cs err)]
    Right (Object o) -> handler o
    Right _ ->
      return $ responseLBS status400 [jsonH] jErr
      where
        jErr = encode . object $
          [("message", String "Expecting a JSON object")]

data TableOptions = TableOptions {
  tblOptcolumns :: [Column]
, tblOptpkey :: [Text]
}

instance ToJSON TableOptions where
  toJSON t = object [
      "columns" .= tblOptcolumns t
    , "pkey"   .= tblOptpkey t ]
