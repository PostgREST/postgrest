module App (app) where

import Control.Monad (join)
import Data.Monoid ( (<>) )
import Control.Arrow ((***))
import Control.Applicative

import Data.Text hiding (map)
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Regex.TDFA ((=~))
import Data.Ord (comparing)
import Data.Ranged.Ranges (emptyRange)
import Data.HashMap.Strict (keys, elems, filterWithKey, toList)
import Data.ByteString.Char8 hiding (zip, map, elem)
import Data.String.Conversions (cs)
import Data.List (sortBy)
import qualified Data.Set as S

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.HTTP.Base (urlEncodeVars)
import Network.Wai

import Data.Aeson
import qualified Hasql as H
import qualified Hasql.Postgres as H

import PgQuery
import RangeQuery
import PgStructure
import Auth

app :: Connection -> Application
app conn req respond =
  respond =<< case (path, verb) of
    ([], _) -> do
      body <- encode <$> tables conn (cs schema)
      return $ responseLBS status200 [jsonH] $ cs body

    ([table], "OPTIONS") -> do
      let t = QualifiedTable schema (cs table)
      cols <- columns conn t
      pkey <- map cs <$> primaryKeyColumns conn t
      return $ responseLBS status200 [jsonH, allOrigins]
        $ encode (TableOptions cols pkey)

    ([table], "GET") ->
      if range == Just emptyRange
      then return $ responseLBS status416 [] "HTTP Range error"
      else do
        let qt = QualifiedTable schema (cs table)
        let select =
              ("select ",[]) <>
                  parentheticT (
                    whereT qq $ countRows qt
                  ) <> commaq <> (
                  asJsonWithCount
                  . limitT range
                  . orderT (orderParse qq)
                  . whereT qq
                  $ selectStar qt
                )
        row <- listToMaybe <$> uncurry (query conn) select
        let (tableTotal, queryTotal, body) =
              fromMaybe (0, 0, Just "" :: Maybe ByteString) row
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
      body <- strictRequestBody req
      let user = decode body :: Maybe AuthUser

      case user of
        Nothing -> return $ responseLBS status400 [jsonH] $
          encode . object $ [("error", String "Failed to parse user.")]
        Just u -> do
          _ <- addUser conn (cs $ userId u)
                 (cs $ userPass u) (cs $ userRole u)
          return $ responseLBS status201
            [ jsonH
            , (hLocation, "/dbapi/users?id=eq." <> cs (userId u))
            ] ""

    ([table], "POST") ->
      handleJsonObj req $ \obj -> do
        let qt = QualifiedTable schema (cs table)
        _  <- uncurry (execute conn)
          $ insertInto qt (map cs $ keys obj) (elems obj)
        primaryKeys <- map cs <$> primaryKeyColumns conn qt
        let primaries = filterWithKey (const . (`elem` primaryKeys)) obj
        let params = urlEncodeVars
              $ map (\t -> (cs $ fst t, "eq." <> cs (encode $ snd t)))
              $ toList primaries
        return $ responseLBS status201
          [ jsonH
          , (hLocation, "/" <> cs table <> "?" <> cs params)
          ] ""

    ([table], "PUT") ->
      handleJsonObj req $ \obj -> do
        let qt = QualifiedTable schema (cs table)
        primaryKeys <- primaryKeyColumns conn qt
        let specifiedKeys = map (cs . fst) qq
        if S.fromList primaryKeys /= S.fromList specifiedKeys
          then return $ responseLBS status405 []
               "You must speficy all and only primary keys as params"
          else do
            tableCols <- map (cs . colName) <$> columns conn qt
            let cols = map cs $ keys obj
            if S.fromList tableCols == S.fromList cols then do
              let vals = elems obj
              _  <- uncurry (execute conn) $ iffNotT
                      (whereT qq $ update qt cols vals)
                      (insertInto qt cols vals)
              return $ responseLBS status204 [ jsonH ] ""

              else return $ if Prelude.null tableCols
                then responseLBS status404 [] ""
                else responseLBS status400 []
                   "You must specify all columns in PUT request"

    ([table], "PATCH") ->
      handleJsonObj req $ \obj -> do
        let qt = QualifiedTable schema (cs table)
        _  <- uncurry (execute conn)
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

requestedSchema :: RequestHeaders -> ByteString
requestedSchema hdrs =
  case verStr of
       Just [[_, ver]] -> ver
       _ -> "1"

  where verRegex = "version[ ]*=[ ]*([0-9]+)" :: String
        accept = lookup hAccept hdrs :: Maybe ByteString
        verStr = (=~ verRegex) <$> accept :: Maybe [[ByteString]]

jsonH :: Header
jsonH = (hContentType, "application/json")

handleJsonObj :: Request -> (Object -> IO Response) -> IO Response
handleJsonObj req handler = do
  parse <- fmap eitherDecode . strictRequestBody $ req
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
