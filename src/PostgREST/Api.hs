{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PostgREST.Api (runTestServer) where

import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Data.String.Conversions (cs)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import           Servant
import Servant.Server

-- * Example

-- | A greet message data type
newtype ResultSet = ResultSet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON ResultSet
instance ToJSON ResultSet

instance MimeRender PlainText ResultSet where
    mimeRender _ = cs . show

data QueryString

instance HasServer api => HasServer (QueryString :> api) where
  -- Type level extension
  type ServerT (QueryString :> api) m =
    [(ByteString, Maybe ByteString)] -> ServerT api m
  -- Implementation
  route Proxy subServer req =
    route (Proxy :: Proxy api) (subServer (queryString req)) req

-- API specification
type Api =
  -- GET /:relation?parameters returns a Resultset as JSON
  Capture "relation" Text :> QueryString :> Header "Prefer" Text :> Get '[JSON, PlainText] ResultSet

  -- POST /:relation with data in request body and returns Resultset as JSON
  :<|> Capture "relation" Text :> ReqBody '[JSON] ResultSet :> Post '[JSON] ResultSet

  -- POST /rpc/:procedure with data in request body and returns Resultset as JSON
  :<|> "rpc" :> Capture "procedure" Text :> ReqBody '[JSON] ResultSet :> Post '[JSON] ResultSet

  -- PATCH /:relation?parameters with data in request body and returns Resultset as JSON
  :<|> Capture "relation" Text :> QueryString :> ReqBody '[JSON] ResultSet :> Patch '[JSON] ResultSet

  -- DELETE /:relation?parameters returns a Resultset as JSON
  :<|> Capture "relation" Text :> Delete '[JSON] ResultSet

testApi :: Proxy Api
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT ServantErr IO' monad.
server :: Server Api
server = readData :<|> insertData :<|> callProc :<|> updateData :<|> removeData
  where 
    readData relation qs prefer = return . ResultSet $ "Reading data"
    insertData relation rows = return . ResultSet $ "Inserting data"
    callProc procedure parameters = return . ResultSet $ "Inserting data"
    updateData relation parameters rows = return . ResultSet $ "Updating data"
    removeData relation = return . ResultSet $ "Removing data"

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Port -> IO ()
runTestServer port = run port test
