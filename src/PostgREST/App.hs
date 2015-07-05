{-# LANGUAGE FlexibleContexts #-}
module PostgREST.App (app, sqlError, isSqlError) where

import Control.Monad (join)
import Control.Arrow ((***), second)
import Control.Applicative

import Data.Text hiding (map)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Regex.TDFA ((=~))
import Data.Ord (comparing)
import Data.Ranged.Ranges (emptyRange)
import qualified Data.HashMap.Strict as M
import Data.String.Conversions (cs)
import Data.CaseInsensitive (original)
import Data.List (sortBy)
import Data.Functor.Identity
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Blaze.ByteString.Builder as BB
import qualified Data.Csv as CSV

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI (parseSimpleQuery)
import Network.HTTP.Base (urlEncodeVars)
import Network.Wai
import Network.Wai.Internal (Response(..))

import Data.Aeson
import Data.Monoid
import qualified Data.Vector as V
import qualified Hasql as H
import qualified Hasql.Backend as B
import qualified Hasql.Postgres as P

import PostgREST.Config (AppConfig(..))
import PostgREST.Auth
import PostgREST.PgQuery
import PostgREST.RangeQuery
import PostgREST.PgStructure

import Prelude

app :: AppConfig -> BL.ByteString -> Request -> H.Tx P.Postgres s Response
app conf reqBody req =
  case (path, verb) of
    ([], _) -> do
      body <- encode <$> tables (cs schema)
      return $ responseLBS status200 [jsonH] $ cs body

    ([table], "OPTIONS") -> do
      let qt = qualify table
      cols <- columns qt
      pkey <- map cs <$> primaryKeyColumns qt
      return $ responseLBS status200 [jsonH, allOrigins]
        $ encode (TableOptions cols pkey)

    ([table], "GET") ->
      if range == Just emptyRange
      then return $ responseLBS status416 [] "HTTP Range error"
      else do
        let qt = qualify table
            select = B.Stmt "select " V.empty True <>
                  parentheticT (
                    whereT qt qq $ countRows qt
                  ) <> commaq <> (
                  asJsonWithCount
                  . limitT range
                  . orderT (orderParse qq)
                  . whereT qt qq
                  $ selectStar qt
                )
        row <- H.maybeEx select
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

    (["postgrest", "tokens"], "POST") ->
      case jwtSecret of
        "secret" -> return $ responseLBS status500 [jsonH] $
          encode . object $ [("message", String "JWT Secret is set as \"secret\" which is an unsafe default.")]
        _ -> do
          let user = decode reqBody :: Maybe AuthUser

          case user of
            Nothing -> return $ responseLBS status400 [jsonH] $
              encode . object $ [("message", String "Failed to parse user.")]
            Just u -> do
              setRole authenticator
              login <- signInRole (cs $ userId u)
                              (cs $ userPass u)
              case login of
                LoginSuccess role uid ->
                  return $ responseLBS status201 [ jsonH ] $
                    encode . object $ [("token", String $ tokenJWT jwtSecret uid role)]
                _  -> return $ responseLBS status401 [jsonH] $
                  encode . object $ [("message", String "Failed authentication.")]

    ([table], "POST") -> do
      let qt = qualify table
          echoRequested = lookup "Prefer" hdrs == Just "return=representation"
          parsed :: Either String (V.Vector Text, V.Vector (V.Vector Value))
          parsed = if lookup "Content-Type" hdrs == Just "text/csv"
                    then do
                      rows <- CSV.decode CSV.NoHeader reqBody
                      if V.null rows then Left "CSV requires header"
                        else Right (V.head rows, (V.map $ V.map $ parseCsvCell . cs) (V.tail rows))
                    else eitherDecode reqBody >>= \val ->
                      case val of
                        Object obj -> Right .  second V.singleton .  V.unzip .  V.fromList $
                          M.toList obj
                        _ -> Left "Expecting single JSON object or CSV rows"
      case parsed of
        Left err -> return $ responseLBS status400 [] $
          encode . object $ [("message", String $ "Failed to parse JSON payload. " <> cs err)]
        Right toBeInserted -> do
          rows :: [Identity Text] <- H.listEx $ uncurry (insertInto qt) toBeInserted
          let inserted :: [Object] = mapMaybe (decode . cs . runIdentity) rows
          primaryKeys <- primaryKeyColumns qt
          let responses = flip map inserted $ \obj -> do
                let primaries =
                      if Prelude.null primaryKeys
                        then obj
                        else M.filterWithKey (const . (`elem` primaryKeys)) obj
                let params = urlEncodeVars
                      $ map (\t -> (cs $ fst t, cs (paramFilter $ snd t)))
                      $ sortBy (comparing fst) $ M.toList primaries
                responseLBS status201
                  [ jsonH
                  , (hLocation, "/" <> cs table <> "?" <> cs params)
                  ] $ if echoRequested then encode obj else ""
          return $ multipart status201 responses

    ([table], "PUT") ->
      handleJsonObj reqBody $ \obj -> do
        let qt = qualify table
        primaryKeys <- primaryKeyColumns qt
        let specifiedKeys = map (cs . fst) qq
        if S.fromList primaryKeys /= S.fromList specifiedKeys
          then return $ responseLBS status405 []
               "You must speficy all and only primary keys as params"
          else do
            tableCols <- map (cs . colName) <$> columns qt
            let cols = map cs $ M.keys obj
            if S.fromList tableCols == S.fromList cols
              then do
                let vals = M.elems obj
                H.unitEx $ iffNotT
                        (whereT qt qq $ update qt cols vals)
                        (insertSelect qt cols vals)
                return $ responseLBS status204 [ jsonH ] ""

              else return $ if Prelude.null tableCols
                then responseLBS status404 [] ""
                else responseLBS status400 []
                   "You must specify all columns in PUT request"

    ([table], "PATCH") ->
      handleJsonObj reqBody $ \obj -> do
        let qt = qualify table
            up = returningStarT
               . whereT qt qq
               $ update qt (map cs $ M.keys obj) (M.elems obj)
            patch = withT up "t" $ B.Stmt
              "select count(t), array_to_json(array_agg(row_to_json(t)))::character varying"
              V.empty True

        row <- H.maybeEx patch
        let (queryTotal, body) =
              fromMaybe (0 :: Int, Just "" :: Maybe Text) row
            r = contentRangeH 0 (queryTotal-1) queryTotal
            echoRequested = lookup "Prefer" hdrs == Just "return=representation"
            s = case () of _ | queryTotal == 0 -> status404
                             | echoRequested -> status200
                             | otherwise -> status204
        return $ responseLBS s [ jsonH, r ] $ if echoRequested then cs $ fromMaybe "[]" body else ""

    ([table], "DELETE") -> do
      let qt = qualify table
      let del = countT
            . returningStarT
            . whereT qt qq
            $ deleteFrom qt
      row <- H.maybeEx del
      let (Identity deletedCount) = fromMaybe (Identity 0 :: Identity Int) row
      return $ if deletedCount == 0
         then responseLBS status404 [] ""
         else responseLBS status204 [("Content-Range", "*/"<> cs (show deletedCount))] ""

    (_, _) ->
      return $ responseLBS status404 [] ""

  where
    path          = pathInfo req
    verb          = requestMethod req
    qq            = queryString req
    qualify       = QualifiedTable schema
    hdrs          = requestHeaders req
    schema        = requestedSchema (cs $ configV1Schema conf) hdrs
    authenticator = cs $ configDbUser conf
    jwtSecret     = cs $ configJwtSecret conf
    range         = rangeRequested hdrs
    allOrigins    = ("Access-Control-Allow-Origin", "*") :: Header

sqlError :: t
sqlError = undefined

isSqlError :: t
isSqlError = undefined

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

requestedSchema :: Text -> RequestHeaders -> Text
requestedSchema v1schema hdrs =
  case verStr of
       Just [[_, ver]] -> if ver == "1" then v1schema else cs ver
       _ -> v1schema

  where verRegex = "version[ ]*=[ ]*([0-9]+)" :: BS.ByteString
        accept = cs <$> lookup hAccept hdrs :: Maybe BS.ByteString
        verStr = (=~ verRegex) <$> accept :: Maybe [[BS.ByteString]]

jsonH :: Header
jsonH = (hContentType, "application/json")

handleJsonObj :: BL.ByteString -> (Object -> H.Tx P.Postgres s Response)
              -> H.Tx P.Postgres s Response
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

parseCsvCell :: BL.ByteString -> Value
parseCsvCell s = if s == "NULL" then Null else String $ cs s

multipart :: Status -> [Response] -> Response
multipart _ [] = responseLBS status204 [] ""
multipart _ [r] = r
multipart s rs =
  responseLBS s [(hContentType, "multipart/mixed; boundary=\"postgrest_boundary\"")] $
    BL.intercalate "\n--postgrest_boundary\n" (map renderResponseBody rs)

  where
    renderHeader :: Header -> BL.ByteString
    renderHeader (k, v) = cs (original k) <> ": " <> cs v

    renderResponseBody :: Response -> BL.ByteString
    renderResponseBody (ResponseBuilder _ headers b) =
      BL.intercalate "\n" (map renderHeader headers)
        <> "\n\n" <> BB.toLazyByteString b
    renderResponseBody _ = error
      "Unable to create multipart response from non-ResponseBuilder"

data TableOptions = TableOptions {
  tblOptcolumns :: [Column]
, tblOptpkey :: [Text]
}

instance ToJSON TableOptions where
  toJSON t = object [
      "columns" .= tblOptcolumns t
    , "pkey"   .= tblOptpkey t ]
