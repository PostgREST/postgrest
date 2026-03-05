{-
Copyright Jake Pittis (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Toxiproxy
    ( getVersion
    , postReset
    , getProxies
    , createProxy
    , getProxy
    , postPopulate
    , updateProxy
    , deleteProxy
    , getToxics
    , createToxic
    , getToxic
    , updateToxic
    , deleteToxic
    , Proxy(..)
    , Toxic(..)
    , Populate(..)
    , Version(..)
    , Stream(..)
    , ToxicType(..)
    , ProxyName(..)
    , ToxicName(..)
    , Host
    , toxiproxyUrl
    , withDisabled
    , withToxic
    , withProxy
    , run
    ) where

import           Control.Exception   (bracket)
import           Control.Monad       (void)
import           Data.Aeson          (FromJSON, FromJSONKey, ToJSON,
                                      Value (String), defaultOptions,
                                      fieldLabelModifier,
                                      genericParseJSON, genericToJSON,
                                      parseJSON, toJSON)
import qualified Data.Char           as Char (toLower)
import           Data.List           (stripPrefix)
import           Data.Map.Strict     (Map)
import qualified Data.Proxy          as Proxy
import           Data.String         (IsString)
import           Data.Text           (Text)
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings,
                                      newManager)
import           Prelude
import           Servant.API         hiding (Host, Stream)
import           Servant.Client

type ToxiproxyAPI =
       "version"  :> Get '[PlainText] Version
  :<|> "reset"    :> Post '[] NoContent
  :<|> "proxies"  :> Get '[JSON] (Map ProxyName Proxy)
  :<|> "proxies"  :> ReqBody '[JSON] Proxy    :> Post '[JSON] Proxy
  :<|> "proxies"  :> Capture "name" ProxyName :> Get '[JSON] Proxy
  :<|> "populate" :> ReqBody '[JSON] [Proxy]  :> Post '[JSON] Populate
  :<|> "proxies"  :> Capture "name" ProxyName :> ReqBody '[JSON] Proxy :> Post '[JSON] Proxy
  :<|> "proxies"  :> Capture "name" ProxyName :> Delete '[] NoContent
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> Get '[JSON] [Toxic]
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> ReqBody '[JSON] Toxic    :> Post '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> Capture "name" ToxicName :> Get '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> Capture "name" ToxicName :> ReqBody '[JSON] Toxic :> Get '[JSON] Toxic
  :<|> "proxies"  :> Capture "name" ProxyName :>
       "toxics"   :> Capture "name" ToxicName :> Delete '[JSON] NoContent

-- | A unique string for identifying a proxy on the server.
newtype ProxyName = ProxyName Text
  deriving (Show, Eq, IsString, Ord, Generic, ToHttpApiData, FromJSONKey)

instance FromJSON ProxyName
instance ToJSON   ProxyName

-- | A unique string for identifying a toxic on a proxy.
newtype ToxicName = ToxicName Text
  deriving (Show, Eq, IsString, Generic, ToHttpApiData)

instance FromJSON ToxicName
instance ToJSON   ToxicName

-- | The version of the Toxiproxy server. This library is fully supported by any version
--   greater or equal to 2.1.3.
newtype Version = Version Text
  deriving (Show, Eq, MimeUnrender PlainText)

-- | A Toxiproxy proxy. It forwards TCP connections between a listen and upstream host.
--   Toxics can be injected into the proxy to simulate network failure.
data Proxy = Proxy
  { proxyName     :: ProxyName
  -- ^ A unique human readable name to identify a proxy.
  , proxyListen   :: Host
  -- ^ The proxy listens on this host:port.
  , proxyUpstream :: Host
  -- ^ The proxy forwards to this upstream host:port.
  , proxyEnabled  :: Bool
  -- ^ Whether a proxy is currently listening / accepting connections.
  , proxyToxics   :: [Toxic]
  -- ^ The toxics currently applied to the proxy. These should not be specified when
  --   initially creating a proxy. They must be created seperately with 'createToxic'
  --   or 'withToxic'.
  } deriving (Show, Eq, Generic)

instance FromJSON Proxy where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "proxy" }

instance ToJSON Proxy where
  toJSON = genericToJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "proxy" }

-- | A host:port pair to represent the entrence of a proxy or the upstream the proxy
--   forwards to. For the best experience, provide 127.0.0.1 instead of localhost.
type Host = Text

-- | A toxic is applied to a proxy. It allows the user to simulate a specified kind of
--   network failure on the proxy.
data Toxic = Toxic
  { toxicName       :: ToxicName
  -- ^ A unique human readable name to identify a toxic.
  , toxicType       :: ToxicType
  -- ^ The type of toxic. For example "latency". Please refer to 'ToxicType' or the
  -- Toxiproxy documentation for more information.
  , toxicStream     :: Stream
  -- ^ The direction on which the toxic is applied. Please refer to 'Stream'.
  , toxicToxicity   :: Float
  -- ^ The strength that the toxic is applied to the proxy. Please refer to the Toxiproxy
  --   documation.
  , toxicAttributes :: Map Text Int
  -- ^ Attributes configure a toxic. They differ based on the 'ToxicType'. Please refer to
  --   the Toxiproxy documentation.
  } deriving (Show, Eq, Generic)

instance FromJSON Toxic where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "toxic" }

instance ToJSON Toxic where
  toJSON = genericToJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "toxic" }

-- | The return value of the 'populate' endpoint.
newtype Populate = Populate { populateProxies :: [Proxy] }
  deriving (Show, Eq, Generic)

instance FromJSON Populate where
  parseJSON = genericParseJSON $
    defaultOptions
      { fieldLabelModifier = stripPrefixJSON "populate" }

-- | A toxic can be applied to the upstream or the downstream of a connection. Upstream is
--   the stream traveling from the connecting client to the upstream server. Downstream is
--   the stream traveling from the upstream server to the connecting client.
data Stream = Upstream | Downstream
  deriving (Show, Eq)

instance ToJSON Stream where
  toJSON Upstream   = String "upstream"
  toJSON Downstream = String "downstream"

instance FromJSON Stream where
  parseJSON (String stream) =
    case stream of
      "upstream"   -> return Upstream
      "downstream" -> return Downstream
      _            -> fail "must be either upstream or downstream"
  parseJSON _ = fail "must be string"

-- | Different toxic types simulate different kinds of failure. Different toxics require
--   different attribute configuration. Please refer to the Toxiproxy documentation.
data ToxicType =
    Latency
  | Bandwidth
  | SlowClose
  | Timeout
  | Slicer
  | LimitData
  | Other Text
  deriving (Show, Eq)

instance ToJSON ToxicType where
  toJSON Latency       = String "latency"
  toJSON Bandwidth     = String "bandwidth"
  toJSON SlowClose     = String "slow_close"
  toJSON Timeout       = String "timeout"
  toJSON Slicer        = String "slicer"
  toJSON LimitData     = String "limit_data"
  toJSON (Other other) = String other

instance FromJSON ToxicType where
  parseJSON (String tt) =
    case tt of
      "latency"   -> return Latency
      "bandwidth" -> return Bandwidth
      "slow_clos" -> return SlowClose
      "timeout"   -> return Timeout
      "slicer"    -> return Slicer
      "limit_dat" -> return LimitData
      other       -> return . Other $ other
  parseJSON _ = fail "toxicType must be string"

stripPrefixJSON :: String -> String -> String
stripPrefixJSON prefix str =
  case stripPrefix prefix str of
    Just (first : rest) -> Char.toLower first : rest
    _                   -> str

toxiproxyAPI :: Proxy.Proxy ToxiproxyAPI
toxiproxyAPI = Proxy.Proxy

-- | Returns the server version number.
getVersion   :: ClientM Version
-- | Enable all proxies and remove all active toxics.
postReset    :: ClientM NoContent
-- | List existing proxies and their toxics.
getProxies   :: ClientM (Map ProxyName Proxy)
-- | Create a new proxy.
createProxy  :: Proxy -> ClientM Proxy
-- | Get a proxy with all its active toxics.
getProxy     :: ProxyName -> ClientM Proxy
-- | Create or replace a list of proxies.
postPopulate :: [Proxy] -> ClientM Populate
-- | Update a proxy's fields.
updateProxy  :: ProxyName -> Proxy -> ClientM Proxy
-- | Delete an existing proxy.
deleteProxy  :: ProxyName -> ClientM NoContent
-- | List active toxics.
getToxics    :: ProxyName -> ClientM [Toxic]
-- | Create a new toxic.
createToxic  :: ProxyName -> Toxic -> ClientM Toxic
-- |  Get an active toxic's fields.
getToxic     :: ProxyName -> ToxicName -> ClientM Toxic
-- | Update an active toxic.
updateToxic  :: ProxyName -> ToxicName -> Toxic -> ClientM Toxic
-- | Remove an active toxic.
deleteToxic  :: ProxyName -> ToxicName -> ClientM NoContent

(getVersion :<|> postReset :<|> getProxies :<|> createProxy :<|> getProxy :<|> postPopulate
            :<|> updateProxy :<|> deleteProxy :<|> getToxics :<|> createToxic :<|> getToxic
            :<|> updateToxic :<|> deleteToxic) = client toxiproxyAPI

-- | The default Toxiproxy service URL.
--   (127.0.0.1:8474)
toxiproxyUrl :: BaseUrl
toxiproxyUrl = BaseUrl Http "127.0.0.1" 8474 ""

-- | A helper for easily querying the Toxiproxy API. Assumes Toxiproxy is running on
--  'toxiproxyUrl'.
--
-- @
-- proxies <- run getProxies
-- @
run :: ClientM a -> IO (Either ClientError a)
run f = do
  mgr <- newManager defaultManagerSettings
  runClientM f (mkClientEnv mgr toxiproxyUrl)

-- | Given an enabled proxy, disable the proxy, run the given action and then re-enable
--   the proxy.
--
--   This is useful for simulating a crashed server or closed connection.
--
-- @
-- connectToMyProxy       -- This will connect.
-- withDisabled myProxy $
--   connectToMyProxy     -- This will get rejected.
-- connectToMyProxy       -- This will connect again.
-- @
withDisabled :: Proxy -> IO a -> IO a
withDisabled proxy f =
  bracket disable enable $ const f
  where
    enable        = const . run $ updateProxy (proxyName proxy) proxy
    disable       = void . run $ updateProxy (proxyName proxy) disabledProxy
    disabledProxy = proxy { proxyEnabled = False }

-- | Given a proxy and a toxic, create the toxic on the proxy, run the given action and
--   then delete the toxic.
--
--   This is useful for running some action with a toxic enabled.
--
-- @
-- withToxic myProxy latencyToxic $
--   sendRequestThroughProxy -- This request will have latency applied to it.
-- @
withToxic :: Proxy -> Toxic -> IO a -> IO a
withToxic proxy toxic f =
  bracket enable disable $ const f
  where
    enable = void . run $ createToxic (proxyName proxy) toxic
    disable = const . run $ deleteToxic (proxyName proxy) (toxicName toxic)

-- | Given a proxy record, create the proxy on the server, run the given action and then
--   delete the proxy off the server.
--
--   This is useful for wrapping 'withDisabled' and 'withToxic' calls. It enures that your
--   test cleans up the Toxiproxy server so that proxies don't leak into your other tests.
withProxy :: Proxy -> (Proxy -> IO a) -> IO a
withProxy proxy =
  bracket create delete
  where
    create = run (createProxy proxy) >> return proxy
    delete = const . run $ deleteProxy (proxyName proxy)
