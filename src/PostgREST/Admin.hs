module PostgREST.Admin
  ( postgrestAdmin
  ) where

import qualified Data.Text as T

import Network.Socket
import Network.Socket.ByteString

import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai               as Wai

import qualified Hasql.Pool    as SQL
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
      else isRight <$> SQL.use (AppState.getPool appState) (SQL.sql "SELECT 1")

  case Wai.pathInfo req of
    ["ready"] ->
      respond $ Wai.responseLBS (if isMainAppReachable && isConnectionUp && isSchemaCacheLoaded then HTTP.status200 else HTTP.status503) [] mempty
    ["live"] ->
      respond $ Wai.responseLBS (if isMainAppReachable then HTTP.status200 else HTTP.status503) [] mempty
    _ ->
      respond $ Wai.responseLBS HTTP.status404 [] mempty

-- Try to connect to the main app socket
-- Note that it doesn't even send a valid HTTP request, we just want to check that the main app is accepting connections
reachMainApp :: AppConfig -> IO [Either IOException ()]
reachMainApp appConfig =
  case configServerUnixSocket appConfig of
    Just path ->  do
      sock <- socket AF_UNIX Stream 0
      connect sock $ SockAddrUnix path
      res <- try . withSocketsDo $ bracket (pure sock) close sendEmpty
      pure [res]
    Nothing -> do
      addrs <-
        let host = if configServerHost appConfig `elem` ["*4", "!4", "*6", "!6"]
                     then Nothing
                     else Just $ configServerHost appConfig in
        getAddrInfo (Just $ defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] })
                             (T.unpack <$> host)
                             (Just . show $ configServerPort appConfig)
      let
        addrs4 = filter (\x -> addrFamily x /= AF_INET6) addrs
        addrs6 = filter (\x -> addrFamily x == AF_INET6) addrs
        addrs' =
          case configServerHost appConfig of
              "*4" -> addrs4 ++ addrs6
              "!4" -> addrs4
              "*6" -> addrs6 ++ addrs4
              "!6" -> addrs6
              _    -> addrs
      tryAddr `traverse` addrs'
  where
    sendEmpty sock = void $ send sock mempty
    tryAddr :: AddrInfo -> IO (Either IOException ())
    tryAddr addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      try . withSocketsDo $ bracket (pure sock) close sendEmpty
