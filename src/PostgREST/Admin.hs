{-# LANGUAGE RecordWildCards #-}
module PostgREST.Admin
  ( postgrestAdmin
  ) where

import qualified Data.Text as T

import Network.Socket
import Network.Socket.ByteString

import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai               as Wai

import qualified Hasql.Session as SQL

import qualified PostgREST.AppState as AppState
import           PostgREST.Config   (AppConfig (..))

import Protolude

-- | PostgREST admin application
postgrestAdmin :: AppState.AppState -> AppConfig -> Wai.Application
postgrestAdmin appState appConfig req respond  = do
  isMainAppReachable  <- any isRight <$> reachMainApp appConfig
  isSchemaCacheLoaded <- isJust <$> AppState.getDbStructure appState
  isConnectionUp      <-
    if configDbChannelEnabled appConfig
      then AppState.getIsListenerOn appState
      else isRight <$> AppState.usePool appState (SQL.sql "SELECT 1")

  case Wai.pathInfo req of
    ["ready"] ->
      respond $ Wai.responseLBS (if isMainAppReachable && isConnectionUp && isSchemaCacheLoaded then HTTP.status200 else HTTP.status503) [] mempty
    ["live"] ->
      respond $ Wai.responseLBS (if isMainAppReachable then HTTP.status200 else HTTP.status503) [] mempty
    _ ->
      respond $ Wai.responseLBS HTTP.status404 [] mempty

-- Try to connect to the main app socket
-- Note that it doesn't even send a valid HTTP request, we just want to check that the main app is accepting connections
-- The code for resolving the "*4", "!4", "*6", "!6", "*" special values is taken from
-- https://hackage.haskell.org/package/streaming-commons-0.2.2.4/docs/src/Data.Streaming.Network.html#bindPortGenEx
reachMainApp :: AppConfig -> IO [Either IOException ()]
reachMainApp AppConfig{..} =
  case configServerUnixSocket of
    Just path ->  do
      sock <- socket AF_UNIX Stream 0
      (:[]) <$> try (do
        connect sock $ SockAddrUnix path
        withSocketsDo $ bracket (pure sock) close sendEmpty)
    Nothing -> do
      let
        host | configServerHost `elem` ["*4", "!4", "*6", "!6", "*"] = Nothing
             | otherwise                                             = Just configServerHost
        filterAddrs xs =
          case configServerHost of
              "*4" -> ipv4Addrs xs ++ ipv6Addrs xs
              "!4" -> ipv4Addrs xs
              "*6" -> ipv6Addrs xs ++ ipv4Addrs xs
              "!6" -> ipv6Addrs xs
              _    -> xs
        ipv4Addrs = filter ((/=) AF_INET6 . addrFamily)
        ipv6Addrs = filter ((==) AF_INET6 . addrFamily)

      addrs <- getAddrInfo (Just $ defaultHints { addrSocketType = Stream }) (T.unpack <$> host) (Just . show $ configServerPort)
      tryAddr `traverse` filterAddrs addrs
  where
    sendEmpty sock = void $ send sock mempty
    tryAddr :: AddrInfo -> IO (Either IOException ())
    tryAddr addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      try $ do
        connect sock $ addrAddress addr
        withSocketsDo $ bracket (pure sock) close sendEmpty
