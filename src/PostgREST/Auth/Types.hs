module PostgREST.Auth.Types
  ( init
  , AuthResult (..)
  , JwtCacheState (..) )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString   as BS
import qualified Data.Cache        as C

import Protolude

-- TODO: Does not quite belong to this module. Needs refactor
-- | Initialize Jwt Cache State
init :: IO JwtCacheState
init = do
  jwtCache <- C.newCache Nothing
  sem <- newQSem 1 -- allow only 1 new thread, others wait
  return $ JwtCacheState jwtCache sem


-- | Parse result for JWT Claims
data AuthResult = AuthResult
  { authClaims :: KM.KeyMap JSON.Value
  , authRole   :: BS.ByteString
  }

-- | Jwt Cache related State
data JwtCacheState = JwtCacheState
  { cache     :: C.Cache BS.ByteString AuthResult
  , semaphore :: QSem
  }
