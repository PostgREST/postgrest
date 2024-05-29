module PostgREST.Network
  ( resolveHost
  ) where

import           Data.IP        (fromHostAddress, fromHostAddress6)
import           Data.String    (IsString (..))
import qualified Network.Socket as NS

import Protolude

resolveHost :: NS.Socket -> IO (Maybe Text)
resolveHost sock = do
  sn <- NS.getSocketName sock
  case sn of
    NS.SockAddrInet _ hostAddr ->  pure $ Just $ fromString $ show $ fromHostAddress hostAddr
    NS.SockAddrInet6 _ _ hostAddr6 _ -> pure $ Just $ fromString $ show $ fromHostAddress6 hostAddr6
    _ -> pure Nothing
