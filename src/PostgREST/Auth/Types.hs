module PostgREST.Auth.Types
  ( AuthResult (..) )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString   as BS

-- | Parse result for JWT Claims
data AuthResult = AuthResult
  { authClaims :: KM.KeyMap JSON.Value
  , authRole   :: BS.ByteString
  }
