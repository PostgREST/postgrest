module PostgREST.Network
  ( resolveSocketToAddress
  , escapeHostName
  , isSpecialHostName
  ) where

import           Data.String    (IsString (..))
import qualified Network.Socket as NS

import Protolude

-- | Resolves the socket to an address depending on the socket type. The Show
--   instance of the socket types automatically resolves it to the correct
--   address. Example resolution:
-- -----------------------------------------------------
-- | IPv4         | IPv6             | Unix            |
-- -----------------------------------------------------
-- | 127.0.0.1:80 | [2001:db8::1]:80 | /tmp/pgrst.sock |
-- -----------------------------------------------------
resolveSocketToAddress :: NS.Socket -> IO Text
resolveSocketToAddress sock = do
  sn <- NS.getSocketName sock
  return $ showSocketAddr sn

-- |
-- >>> let addr_ipv4 = NS.SockAddrInet 80 (NS.tupleToHostAddress (127,0,0,1))
-- >>> let addr_ipv6 = NS.SockAddrInet6 80 0 (0,0,0,1) 0
-- >>> let addr_unix = NS.SockAddrUnix "/tmp/pgrst.sock"
--
-- >>> showSocketAddr addr_ipv4
-- "127.0.0.1:80"

-- >>> showSocketAddr addr_ipv6
-- "[::1]:80"
--
-- >>> showSocketAddr addr_unix
-- "/tmp/pgrst.sock"
showSocketAddr :: NS.SockAddr -> Text
showSocketAddr = fromString . show

-- | When printing special addresses like !4 or *6, we use the following mapping.
--   These special addresses come from:
--     https://hackage.haskell.org/package/streaming-commons-0.2.3.0/docs/\
--     Data-Streaming-Network.html#t:HostPreference
-- TODO: "!6" should not be printed as "0.0.0.0" address.
escapeHostName :: Text -> Text
escapeHostName "*"  = "0.0.0.0"
escapeHostName "*4" = "0.0.0.0"
escapeHostName "!4" = "0.0.0.0"
escapeHostName "*6" = "0.0.0.0"
escapeHostName "!6" = "0.0.0.0"
escapeHostName h    = h

-- | Check if a hostname is special
isSpecialHostName :: Text -> Bool
isSpecialHostName "*"  = True
isSpecialHostName "*4" = True
isSpecialHostName "!4" = True
isSpecialHostName "*6" = True
isSpecialHostName "!6" = True
isSpecialHostName _    = False
