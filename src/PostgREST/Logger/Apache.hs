module PostgREST.Logger.Apache
  ( apacheFormat
  ) where

import qualified Data.ByteString.Char8 as BS
import           Network.Wai.Logger
import           System.Log.FastLogger

import Network.HTTP.Types.Status (Status, statusCode)
import Network.Wai

import Protolude

apacheFormat :: ToLogStr user => (Request -> Maybe user) -> FormattedTime -> Request -> Status -> Maybe Integer -> ByteString
apacheFormat userget tmstr req status msize =
  fromLogStr $ apacheLogStr userget tmstr req status msize

-- This code is vendored from
-- https://github.com/kazu-yamamoto/logger/blob/57bc4d3b26ca094fd0c3a8a8bb4421bcdcdd7061/wai-logger/Network/Wai/Logger/Apache.hs#L44-L45
apacheLogStr :: ToLogStr user => (Request -> Maybe user) -> FormattedTime -> Request -> Status -> Maybe Integer -> LogStr
apacheLogStr userget tmstr req status msize =
      toLogStr (getSourceFromSocket req)
  <> " - "
  <> maybe "-" toLogStr (userget req)
  <> " ["
  <> toLogStr tmstr
  <> "] \""
  <> toLogStr (requestMethod req)
  <> " "
  <> toLogStr path
  <> " "
  <> toLogStr (show (httpVersion req)::Text)
  <> "\" "
  <> toLogStr (show (statusCode status)::Text)
  <> " "
  <> toLogStr (maybe "-" show msize::Text)
  <> " \""
  <> toLogStr (fromMaybe "" mr)
  <> "\" \""
  <> toLogStr (fromMaybe "" mua)
  <> "\"\n"
  where
    path = rawPathInfo req <> rawQueryString req
    mr  = requestHeaderReferer req
    mua = requestHeaderUserAgent req

getSourceFromSocket :: Request -> ByteString
getSourceFromSocket = BS.pack . showSockAddr . remoteHost
