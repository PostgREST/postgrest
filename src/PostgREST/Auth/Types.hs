module PostgREST.Auth.Types
  ( AuthResult (..) )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString   as BS

import Protolude

-- |
-- Parse and store result for JWT Claims. Can be accessed in
-- db through GUCs (for RLS etc)
data AuthResult = AuthResult
  { authClaims :: KM.KeyMap JSON.Value
  , authRole   :: BS.ByteString
  , authSchema :: Maybe BS.ByteString
  }
