{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

module Main where

import Control.Applicative
import Control.Exception (try)

import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Types (SqlError, seErrorMsg)

import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import Options.Applicative hiding (columns)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import PgStructure (printTables, printColumns)
import PgQuery
import RangeQuery

import Data.Maybe (fromMaybe)
import Text.Regex.TDFA ((=~))
import Text.Read (readMaybe)
import Data.Text (unpack)

import Data.Ranged.Ranges (emptyRange)

import Debug.Trace

-- }}}

data AppConfig = AppConfig {
    configDbUri :: String
  , configPort  :: Int }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> strOption (long "db" <> short 'd' <> metavar "URI"
    <> help "database uri to expose, e.g. postgres://user:pass@host:port/database")
  <*> option (long "port" <> short 'p' <> metavar "NUMBER" <> value 3000
    <> help "port number on which to run HTTP server")

main :: IO ()
main = do
  conf <- execParser (info (helper <*> argParser) describe)

  Prelude.putStrLn $ "Listening on port " ++ (show $ configPort conf :: String)
  run (configPort conf) $ app conf

  where
    describe = progDesc "create a REST API to an existing Postgres database"

traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x

app ::  AppConfig -> Application
app config req respond = do
  r <- try $
    case (path, verb) of
      ([], _) ->
        responseLBS status200 [json] <$> (printTables ver =<< conn)
      ([table], "OPTIONS") ->
        responseLBS status200 [json] <$> (printColumns ver (unpack table) =<< conn)
      ([table], "GET") ->
        if range == Just emptyRange
        then return $ responseLBS status416 [] "HTTP Range error"
        else responseLBS status200 [json] . rrBody <$>
          (getRows (show ver) (unpack table) qq range =<< conn)
      (_, _) ->
        return $ responseLBS status404 [] ""


  respond $ either sqlErrorHandler id r

  where
    path   = pathInfo req
    verb   = requestMethod req
    json   = (hContentType, "application/json")
    conn   = connectPostgreSQL $ configDbUri config
    qq     = queryString req
    ver    = fromMaybe 1 $ requestedVersion (requestHeaders req)
    range  = requestedRange (requestHeaders req)

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
