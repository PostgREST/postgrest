module PostgREST.Auth.Types
  ( AuthResult (..) )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString   as BS
import qualified Data.Cache        as C

-- | Parse result for JWT Claims
data AuthResult = AuthResult
  { authClaims :: KM.KeyMap JSON.Value
  , authRole   :: BS.ByteString
  }

-- | JWT Cache and lock to prevent multiple purging threads
data JwtCacheState = JwtCacheState
  { cache     :: C.Cache BS.ByteString AuthResult
  , purgeLock :: MVar ()}
