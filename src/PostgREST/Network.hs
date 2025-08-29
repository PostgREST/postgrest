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
    -- The IPv6 addresses are wrapped in [] brackets. This is done in accordance
    -- to RFC 3986 (https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.2).
    -- In short, we did this to have a clear separation between the port and host
    -- because the components of an IPv6 are separated with the ':' character.
    NS.SockAddrInet6 _ _ hostAddr6 _ -> pure $ Just $ fromString $ "[" ++ show (fromHostAddress6 hostAddr6) ++ "]"
    _ -> pure Nothing
