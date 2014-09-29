{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports
module Dbapi where

import Types (SqlRow, getRow)

import Control.Exception (try)
import Control.Monad (join)
import Control.Arrow ((***))
import Control.Applicative
import Options.Applicative hiding (columns)

import Data.Maybe (fromMaybe, isJust)
import Text.Regex.TDFA ((=~))
import Data.Map (intersection, fromList, toList, Map)
import Data.List (sort)
import qualified Data.Set as S
import Data.Convertible.Base (convert)

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI

import Network.HTTP.Base (urlEncodeVars)

import Network.Wai
import Network.Wai.Internal

import qualified Data.ByteString.Char8 as BS
import Data.String.Conversions (cs)

import Database.HDBC.PostgreSQL (Connection)
import Database.HDBC.Types (SqlError, seErrorMsg)
import PgStructure (printTables, printColumns, primaryKeyColumns,
                    columns, Column(colName))

import qualified Data.Aeson as JSON

import PgQuery
import RangeQuery
import Data.Ranged.Ranges (emptyRange)

-- }}}

data AppConfig = AppConfig {
    configDbUri :: String
  , configPort  :: Int
  , configSslCert :: FilePath
  , configSslKey  :: FilePath
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

app :: Connection -> Application
app conn req respond = do
  r <- try $
    case (path, verb) of
      ([], _) ->
        responseLBS status200 [jsonContentType] <$> printTables ver conn

      ([table], "OPTIONS") ->
        responseLBS status200 [jsonContentType] <$>
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
                if colNames == specifiedCols then do
                  allvals <- upsert ver table row qq conn
                  let params = urlEncodeVars $ map (\t -> (fst t, "eq." <> convert (snd t) :: String)) $ toList $ filterByKeys allvals keys
                  return $ responseLBS status201
                    [ jsonContentType
                    , (hLocation, "/" <> cs table <> "?" <> cs params)
                    ] ""

                  else return $ if S.null colNames then responseLBS status404 [] ""
                    else responseLBS status400 []
                       "You must specify all columns in PUT request"
        )

      (_, _) ->
        return $ responseLBS status404 [] ""

  respond $ either sqlErrorHandler id r

  where
    path   = pathInfo req
    verb   = requestMethod req
    qq     = queryString req
    ver    = fromMaybe "1" $ requestedVersion (requestHeaders req)
    range  = requestedRange (requestHeaders req)
    cRange = requestedContentRange (requestHeaders req)

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

sqlErrorHandler :: SqlError -> Response
sqlErrorHandler e =
  responseLBS status400 [] $ cs (seErrorMsg e)

addHeaders :: ResponseHeaders -> Response -> Response
addHeaders hdrs (ResponseFile    s headers fp m) =
                 ResponseFile    s (headers ++ hdrs) fp m
addHeaders hdrs (ResponseBuilder s headers b)    =
                 ResponseBuilder s (headers ++ hdrs) b
addHeaders hdrs (ResponseStream  s headers b)    =
                 ResponseStream  s (headers ++ hdrs) b
addHeaders hdrs (ResponseRaw     s resp)         =
                 ResponseRaw     s (addHeaders hdrs resp)
