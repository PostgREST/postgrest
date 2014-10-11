{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports
module Dbapi where

import Types (SqlRow, getRow)
import Middleware(reportPgErrors)

import Control.Monad (join)
import Control.Exception.Base (bracket_)
import Control.Arrow ((***))
import Control.Applicative
import Options.Applicative hiding (columns)

import Data.Maybe (fromMaybe, isJust)
import Text.Regex.TDFA ((=~))
import Data.Map (intersection, fromList, toList, Map)
import Data.List (sort)
import qualified Data.Set as S
import Data.Convertible.Base (convert)
import Data.Text (strip)

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI

import Network.HTTP.Base (urlEncodeVars)

import Network.Wai
import Network.Wai.Internal
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..))

import qualified Data.ByteString.Char8 as BS
import Data.String.Conversions (cs)
import qualified Data.CaseInsensitive as CI

import Database.HDBC.PostgreSQL (Connection)
import PgStructure (printTables, printColumns, primaryKeyColumns,
                    columns, Column(colName))

import qualified Data.Aeson as JSON

import PgQuery
import RangeQuery
import Data.Ranged.Ranges (emptyRange)
import Codec.Binary.Base64.String (decode)

-- }}}

data AppConfig = AppConfig {
    configDbUri :: String
  , configPort  :: Int
  , configSslCert :: FilePath
  , configSslKey  :: FilePath
  , configAnonRole :: String
  }

jsonContentType :: (HeaderName, BS.ByteString)
jsonContentType = (hContentType, "application/json")

jsonBodyAction :: Request -> (SqlRow -> IO Response) -> IO Response
jsonBodyAction req handler = do
  parse <- jsonBody req
  case parse of
    Left err -> return $ responseLBS status400 [jsonContentType] json
      where json = JSON.encode . JSON.object $ [("error", JSON.String $ cs err)]
    Right body -> handler body

jsonBody :: Request -> IO (Either String SqlRow)
jsonBody = fmap JSON.eitherDecode . strictRequestBody

filterByKeys :: Ord a => Map a b -> [a] -> Map a b
filterByKeys m keys =
  if null keys then m else
    m `intersection` fromList (zip keys $ repeat undefined)

httpRequesterRole :: RequestHeaders -> Connection -> IO LoginAttempt
httpRequesterRole hdrs conn = do
  let auth = fromMaybe "" $ lookup hAuthorization hdrs
  case BS.split ' ' (cs auth) of
    ("Basic" : b64 : _) ->
      case BS.split ':' $ cs (decode $ cs b64) of
        (u:p:_) -> signInRole u p conn
        _ -> return MalformedAuth
    _ -> return NoCredentials


app :: DbRole -> Connection -> Application
app anonymous conn req respond = do
  attempt <- httpRequesterRole (requestHeaders req) conn

  case attempt of
    MalformedAuth ->
      respond $ responseLBS status400 [] "Malformed basic auth header"
    LoginFailed ->
      respond $ responseLBS status401 [] "Invalid username or password"
    LoginSuccess role ->
      bracket_ (pgSetRole conn role) (pgResetRole conn) $ appWithRole conn req respond
    NoCredentials ->
      bracket_ (pgSetRole conn anonymous) (pgResetRole conn) $ appWithRole conn req respond


appWithRole :: Connection -> Application
appWithRole conn = reportPgErrors (\req respond ->
  let
      path   = pathInfo req
      verb   = requestMethod req
      qq     = queryString req
      hdrs   = requestHeaders req
      ver    = fromMaybe "1" $ requestedVersion hdrs
      range  = requestedRange hdrs
      cRange = requestedContentRange hdrs
      allOrigins = ("Access-Control-Allow-Origin", "*") :: Header
   in respond =<< case (path, verb) of
    ([], _) ->
      responseLBS status200 [jsonContentType] <$> printTables ver conn

    ([table], "OPTIONS") ->
      responseLBS status200 [jsonContentType, allOrigins] <$>
        printColumns ver (cs table) conn

    ([table], "GET") ->
      if range == Just emptyRange
      then return $ responseLBS status416 [] "HTTP Range error"
      else do
        r <- respondWithRangedResult <$> getRows ver (cs table) qq range conn
        let canonical = urlEncodeVars $ sort $
                        map (join (***) cs) $
                        parseSimpleQuery $
                        rawQueryString req
        return $ addHeaders [
          ("Content-Location",
           "/" <> cs table <> "?" <> cs canonical
          )] r

    ([table], "POST") ->
      jsonBodyAction req (\row -> do
        allvals <- insert ver table row conn
        keys <- primaryKeyColumns ver (cs table) conn
        let params = urlEncodeVars $ map (\t -> (fst t, "eq." <> convert (snd t) :: String)) $ toList $ filterByKeys allvals keys
        return $ responseLBS status201
          [ jsonContentType
          , (hLocation, "/" <> cs table <> "?" <> cs params)
          ] ""
      )

    ([table], "PUT") ->
      jsonBodyAction req (\row -> do
        keys <- primaryKeyColumns ver (cs table) conn
        let specifiedKeys = map (cs . fst) qq
        if S.fromList keys /= S.fromList specifiedKeys
          then return $ responseLBS status405 []
               "You must speficy all and only primary keys as params"
          else
            if isJust cRange
               then return $ responseLBS status400 []
                    "Content-Range is not allowed in PUT request"
            else do
              cols <- columns ver (cs table) conn
              let colNames = S.fromList $ map (cs . colName) cols
              let specifiedCols = S.fromList $ map fst $ getRow row
              return $ if colNames == specifiedCols then
                responseLBS status200 [ jsonContentType ] ""

                else if S.null colNames then responseLBS status404 [] ""
                  else responseLBS status400 []
                     "You must specify all columns in PUT request"
      )

    (_, _) ->
      return $ responseLBS status404 [] ""
  )

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True)
    , corsRequestHeaders = "Authentication":accHeaders
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . cs . strip . cs) $ BS.split ',' hdrs
      Nothing -> []


respondWithRangedResult :: RangedResult -> Response
respondWithRangedResult rr =
  responseLBS status [
    jsonContentType,
    ("Content-Range",
      if total == 0 || from > total
      then "*/" <> cs (show total)
      else cs (show from)  <> "-"
         <> cs (show to)    <> "/"
         <> cs (show total)
    )
  ] (rrBody rr)

  where
    from   = rrFrom rr
    to     = rrTo   rr
    total  = rrTotal rr
    status
      | from > total            = status416
      | total == 0               = status204
      | (1 + to - from) < total = status206
      | otherwise               = status200

requestedVersion :: RequestHeaders -> Maybe String
requestedVersion hdrs =
  case verStr of
       Just [[_, ver]] -> Just ver
       _ -> Nothing

  where verRegex = "version[ ]*=[ ]*([0-9]+)" :: String
        accept = cs <$> lookup hAccept hdrs :: Maybe String
        verStr = (=~ verRegex) <$> accept :: Maybe [[String]]


addHeaders :: ResponseHeaders -> Response -> Response
addHeaders hdrs (ResponseFile    s headers fp m) =
                 ResponseFile    s (headers ++ hdrs) fp m
addHeaders hdrs (ResponseBuilder s headers b)    =
                 ResponseBuilder s (headers ++ hdrs) b
addHeaders hdrs (ResponseStream  s headers b)    =
                 ResponseStream  s (headers ++ hdrs) b
addHeaders hdrs (ResponseRaw     s resp)         =
                 ResponseRaw     s (addHeaders hdrs resp)
