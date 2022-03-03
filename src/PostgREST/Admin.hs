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
  isMainAppReachable  <- isRight <$> reachMainApp appConfig
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
reachMainApp :: AppConfig -> IO (Either IOException ())
reachMainApp appConfig =
  try . withSocketsDo $ bracket open close sendEmpty
  where
    open = case configServerUnixSocket appConfig of
      Just path ->  do
        sock <- socket AF_UNIX Stream 0
        connect sock $ SockAddrUnix path
        return sock
      Nothing -> do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just . T.unpack $ configServerHost appConfig) (Just . show $ configServerPort appConfig)
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    sendEmpty sock = void $ send sock mempty
