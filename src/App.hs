{-# LANGUAGE FlexibleContexts #-}
module App (app, sqlErrHandler, isSqlError) where

import Control.Monad (join)
import Control.Arrow ((***))
import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
-- import Control.Exception.Base

import Data.Text hiding (map)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA ((=~))
import Data.Ord (comparing)
import Data.Ranged.Ranges (emptyRange)
import Data.HashMap.Strict (keys, elems, filterWithKey, toList)
import Data.String.Conversions (cs)
import Data.List (sortBy)
import qualified Data.Set as S

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.HTTP.Base (urlEncodeVars)
import Network.Wai

import Data.Aeson
import Data.Coerce
import Data.Monoid
import qualified Hasql as H
import qualified Hasql.Backend as HB
import qualified Hasql.Postgres as H

import PgQuery
import RangeQuery
import PgStructure
import Auth

app :: Request -> H.Session H.Postgres IO Response
app req =
  case (path, verb) of
    ([], _) -> do
      body <- H.tx Nothing $ encode <$> tables (cs schema)
      return $ responseLBS status200 [jsonH] $ cs body

    ([table], "OPTIONS") -> do
      let t = QualifiedTable schema (cs table)
      H.tx Nothing $ do
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
        row <- H.tx Nothing $ H.single select
        let (tableTotal, queryTotal, body) =
              fromMaybe (0, 0, Just "" :: Maybe Text) row
            from = fromMaybe 0 $ rangeOffset <$> range
            to = from+queryTotal
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

    (["dbapi", "users"], "POST") -> do
      body <- liftIO $ strictRequestBody req
      let user = decode body :: Maybe AuthUser

      case user of
        Nothing -> return $ responseLBS status400 [jsonH] $
          encode . object $ [("error", String "Failed to parse user.")]
        Just u -> do
          _ <- liftIO $ addUser (cs $ userId u)
            (cs $ userPass u) (cs $ userRole u)
          return $ responseLBS status201
            [ jsonH
            , (hLocation, "/dbapi/users?id=eq." <> cs (userId u))
            ] ""

    ([table], "POST") ->
      handleJsonObj req $ \obj -> H.tx Nothing $ do
        let qt = QualifiedTable schema (cs table)
        H.unit . coerce $ insertInto qt (map cs $ keys obj) (elems obj)
        primaryKeys <- map cs <$> primaryKeyColumns qt
        let primaries = filterWithKey (const . (`elem` primaryKeys)) obj
        let params = urlEncodeVars
              $ map (\t -> (cs $ fst t, "eq." <> cs (encode $ snd t)))
              $ toList primaries
        return $ responseLBS status201
          [ jsonH
          , (hLocation, "/" <> cs table <> "?" <> cs params)
          ] ""

    ([table], "PUT") ->
      handleJsonObj req $ \obj -> H.tx Nothing $ do
        let qt = QualifiedTable schema (cs table)
        primaryKeys <- primaryKeyColumns qt
        let specifiedKeys = map (cs . fst) qq
        if S.fromList primaryKeys /= S.fromList specifiedKeys
          then return $ responseLBS status405 []
               "You must speficy all and only primary keys as params"
          else do
            tableCols <- map (cs . colName) <$> columns qt
            let cols = map cs $ keys obj
            if S.fromList tableCols == S.fromList cols then do
              let vals = elems obj
              H.unit . coerce $ iffNotT
                      (whereT qq $ update qt cols vals)
                      (insertInto qt cols vals)
              return $ responseLBS status204 [ jsonH ] ""

              else return $ if Prelude.null tableCols
                then responseLBS status404 [] ""
                else responseLBS status400 []
                   "You must specify all columns in PUT request"

    ([table], "PATCH") ->
     handleJsonObj req $ \obj -> H.tx Nothing $ do
        let qt = QualifiedTable schema (cs table)
        H.unit
          $ coerce
          $ whereT qq
          $ update qt (map cs $ keys obj) (elems obj)
        return $ responseLBS status204 [ jsonH ] ""

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


isSqlError :: HB.Error -> Maybe HB.Error
isSqlError (HB.ErroneousResult x) = Just $ HB.ErroneousResult x
isSqlError _ = Nothing

sqlErrHandler :: HB.Error -> IO Response
sqlErrHandler (HB.ErroneousResult err) = do
  return $ if "42P01" `isInfixOf` err
    then responseLBS status404 [] ""
    else responseLBS status400 [] (cs err)
sqlErrHandler _ = error "just for debugging"

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

handleJsonObj :: MonadIO m => Request -> (Object -> m Response) -> m Response
handleJsonObj req handler = do
  parse <- liftIO $ fmap eitherDecode . strictRequestBody $ req
  case parse of
    Left err ->
      return $ responseLBS status400 [jsonH] jErr
      where
        jErr = encode . object $
          [("error", String $ "Failed to parse JSON payload. " <> cs err)]
    Right (Object o) -> handler o
    Right _ ->
      return $ responseLBS status400 [jsonH] jErr
      where
        jErr = encode . object $
          [("error", String "Expecting a JSON object")]

data TableOptions = TableOptions {
  tblOptcolumns :: [Column]
, tblOptpkey :: [Text]
}

instance ToJSON TableOptions where
  toJSON t = object [
      "columns" .= tblOptcolumns t
    , "pkey"   .= tblOptpkey t ]
