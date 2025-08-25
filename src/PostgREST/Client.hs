{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.Client
  ( ready
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Network.HTTP.Client  as HC

import Network.HTTP.Types.Status (Status (..))
import PostgREST.Config          (AppConfig (..))

import Protolude

ready :: AppConfig -> IO Status
ready AppConfig{configAdminServerHost, configAdminServerPort} = do

  client <- HC.newManager HC.defaultManagerSettings
  req <- HC.parseRequest $ -- Create HTTP Request
    case configAdminServerPort of
      Just port -> T.unpack (address port) <> "/ready"
      Nothing   -> panic "Admin Server is not running. Please check your configuration."

  resp <- HC.httpLbs req client     -- HTTP Response
  LBS.putStr $ HC.responseBody resp -- Print Reponse Body
  return $ HC.responseStatus resp
    where
      address port = "http://"
                   <> escapeHostName configAdminServerHost
                   <> ":"
                   <> show port

      escapeHostName :: Text -> Text
      escapeHostName "*"  = "0.0.0.0"
      escapeHostName "*4" = "0.0.0.0"
      escapeHostName "!4" = "0.0.0.0"
      escapeHostName "*6" = "0.0.0.0"
      escapeHostName "!6" = "0.0.0.0"
      escapeHostName h    = h
