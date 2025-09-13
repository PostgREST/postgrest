{-|
Module      : PostgREST.Client
Description : PostgREST HTTP client
-}
{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.Client
  ( ready
  ) where

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Network.HTTP.Client       as HC
import qualified Network.HTTP.Types.Status as HTTP

import Network.HTTP.Client (HttpException (..))
import System.IO           (hFlush)

import PostgREST.Config  (AppConfig (..))
import PostgREST.Network (isSpecialHostName)

import Protolude

data ClientError
  = NoAdminServer
  | NoSpecialHostNamesAllowed Text
  | PostgRESTNotReady         Text
  | HTTPConnectionRefused     Text
  | HTTPExceptionInvalidURL   Text


ready :: AppConfig -> IO ()
ready AppConfig{configAdminServerHost, configAdminServerPort} = do

  client <- HC.newManager HC.defaultManagerSettings
  readyURL <- getURL
  req <- HC.parseRequest $ T.unpack readyURL
  resp <- HC.httpLbs req client `catch` handleHttpException

  let status = HC.responseStatus resp

  if status >= HTTP.status200 && status < HTTP.status300
    then printAndExit ExitSuccess $ "OK: " <> readyURL
    else printAndExit (ExitFailure 1) $ clientErrorMsg (PostgRESTNotReady readyURL)
        where
          getURL :: IO Text
          getURL = case (isSpecialHostName configAdminServerHost, configAdminServerPort) of
              (_, Nothing)       -> printAndExit (ExitFailure 1) $ clientErrorMsg NoAdminServer
              (True, Just _)     -> printAndExit (ExitFailure 1) $ clientErrorMsg (NoSpecialHostNamesAllowed configAdminServerHost)
              (False, Just port) -> return $ "http://" <> configAdminServerHost <> ":" <> (T.pack . show) port <> "/ready"
              -- TODO: For IPv6, the hostname has to be wrapped in []

-- | Handle HTTP exception for "http-client" requests
handleHttpException :: HttpException -> IO (HC.Response LBS.ByteString)
handleHttpException (HttpExceptionRequest req _) = do
  let url = show (HC.getUri req)
  printAndExit (ExitFailure 1) $ clientErrorMsg (HTTPConnectionRefused $ T.pack url)
handleHttpException (InvalidUrlException url _) = do
  printAndExit (ExitFailure 1) $ clientErrorMsg (HTTPExceptionInvalidURL $ T.pack url)

-- | Print the message on given handle and exit
printAndExit :: ExitCode -> Text -> IO a
printAndExit ExitSuccess msg = putStrLn (T.unpack msg) >> hFlush stdout >> exitSuccess
printAndExit (ExitFailure _) msg = hPutStrLn stderr (T.unpack msg) >> hFlush stderr >> exitWith (ExitFailure 1)

-- | ClientError to error message
clientErrorMsg :: ClientError -> Text
clientErrorMsg NoAdminServer                 = "UNREACHABLE: admin server is not running"
clientErrorMsg (NoSpecialHostNamesAllowed host)
  = "REFUSED: The --ready flag doesn't work with special hostnames like \"" <> host <> "\"."
clientErrorMsg (PostgRESTNotReady url)       = "UNAVAILABLE: " <> url
clientErrorMsg (HTTPConnectionRefused url)   = "HTTP EXCEPTION: connection refused to " <> url
clientErrorMsg (HTTPExceptionInvalidURL url) = "HTTP EXCEPTION: invalid url - " <> url
