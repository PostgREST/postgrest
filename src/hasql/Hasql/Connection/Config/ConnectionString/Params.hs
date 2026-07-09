module Hasql.Connection.Config.ConnectionString.Params where

import qualified Data.Map.Strict as Map
import           Hasql.Prelude

type Params = Map.Map ByteString ByteString

class Updates a where
  update :: a -> Params -> Params

nil :: Params
nil = Map.empty

fromUpdates :: (Updates a) => [a] -> Params
fromUpdates = foldl' (flip update) nil

setKeyValue :: ByteString -> ByteString -> Params -> Params
setKeyValue = Map.insert
