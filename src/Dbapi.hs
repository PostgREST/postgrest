{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports
module Dbapi where

import Types (SqlRow)

import Control.Exception (try)
import Control.Applicative
import Options.Applicative hiding (columns)

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import Network.Wai

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import Database.HDBC.PostgreSQL (connectPostgreSQL')
import Database.HDBC.Types (SqlError, seErrorMsg)
import PgStructure (printTables, printColumns)

import qualified Data.Aeson as JSON
import Data.Text (pack, unpack)

import PgQuery
import RangeQuery
import Data.Ranged.Ranges (emptyRange)

-- }}}

data AppConfig = AppConfig {
    configDbUri :: String
  , configPort  :: Int }

jsonContentType :: (HeaderName, BS.ByteString)
jsonContentType = (hContentType, "application/json")

jsonBodyAction :: Request -> (SqlRow -> IO Response) -> IO Response
jsonBodyAction req handler = do
  parse <- jsonBody req
  case parse of
    Left err -> return $ responseLBS status400 [jsonContentType] json
      where json = JSON.encode . JSON.object $ [("error", JSON.String $ pack err)]
    Right body -> handler body

jsonBody :: Request -> IO (Either String SqlRow)
jsonBody = (fmap JSON.eitherDecode) . strictRequestBody

app ::  AppConfig -> Application
app config req respond = do
  conn <- connectPostgreSQL' $ configDbUri config
  r <- try $
    case (path, verb) of
      ([], _) ->
        responseLBS status200 [jsonContentType] <$> (printTables ver conn)
      ([table], "OPTIONS") ->
        responseLBS status200 [jsonContentType] <$> (
          printColumns ver (unpack table) conn)
      ([table], "GET") ->
        if range == Just emptyRange
        then return $ responseLBS status416 [] "HTTP Range error"
        else respondWithRangedResult <$>
          (getRows (show ver) (unpack table) qq range conn)
      ([table], "POST") ->
        jsonBodyAction req (\row ->
          responseLBS status200 [jsonContentType] <$> (
            insert ver table row conn))
      (_, _) ->
        return $ responseLBS status404 [] ""

  respond $ either sqlErrorHandler id r

  where
    path   = pathInfo req
    verb   = requestMethod req
    qq     = queryString req
    ver    = fromMaybe 1 $ requestedVersion (requestHeaders req)
    range  = requestedRange (requestHeaders req)

respondWithRangedResult :: RangedResult -> Response
respondWithRangedResult rr =
  responseLBS status206 [
    jsonContentType,
    ("Content-Range",
      if rrTotal rr == 0
      then "*/0"
      else (BS.pack . show . rrFrom ) rr <> "-"
         <> (BS.pack . show . rrTo   ) rr <> "/"
         <> (BS.pack . show . rrTotal) rr
    )
  ] (rrBody rr)

requestedVersion :: RequestHeaders -> Maybe Int
requestedVersion hdrs =
  case verStr of
       Just [[_, ver]] -> readMaybe ver
       _ -> Nothing

  where verRegex = "version[ ]*=[ ]*([0-9]+)" :: String
        accept = BS.unpack <$> lookup hAccept hdrs :: Maybe String
        verStr = (=~ verRegex) <$> accept :: Maybe [[String]]

sqlErrorHandler :: SqlError -> Response
sqlErrorHandler e =
  responseLBS status400 [] $ BL.fromChunks [BS.pack (seErrorMsg e)]
