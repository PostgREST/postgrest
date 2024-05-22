{- This file is a minimal version of Auth.hs to break the dependency cycle between
 - Auth.hs and AppState.hs. It MUST:
 - * contain only those definitions which are imported in AppState.hs
 - * define those exactly the same way as Auth.hs
-}
module PostgREST.Auth where

import qualified Data.Aeson.KeyMap               as KM
import qualified Data.Aeson.Types                as JSON
import qualified Data.ByteString                 as BS

data Result = Result
  { claims :: KM.KeyMap JSON.Value
  , role   :: BS.ByteString
  }
