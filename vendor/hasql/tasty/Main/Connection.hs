module Main.Connection where

import qualified Hasql.Connection           as HC
import qualified Hasql.TestingKit.Constants as Constants
import           Main.Prelude

with :: (HC.Connection -> IO a) -> IO (Either HC.ConnectionError a)
with handler =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ HC.acquire Constants.localConnectionSettings
    use connection =
      lift $ handler connection
    release connection =
      lift $ HC.release connection
