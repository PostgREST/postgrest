module Hasql.Pool.Config.Config where

import Hasql.Pool.Observation (Observation)
import Hasql.Pool.Prelude

import Hasql.Connection.Setting qualified as Connection.Setting
import Hasql.Pool.Config.Defaults qualified as Defaults

-- | Configuration for Hasql connection pool.
data Config = Config
  { size :: Int
  , acquisitionTimeout :: DiffTime
  , agingTimeout :: DiffTime
  , idlenessTimeout :: DiffTime
  , connectionSettings :: [Connection.Setting.Setting]
  , observationHandler :: Observation -> IO ()
  }

-- | Reasonable defaults, which can be built upon.
defaults :: Config
defaults =
  Config
    { size = Defaults.size
    , acquisitionTimeout = Defaults.acquisitionTimeout
    , agingTimeout = Defaults.agingTimeout
    , idlenessTimeout = Defaults.idlenessTimeout
    , connectionSettings = Defaults.staticConnectionSettings
    , observationHandler = Defaults.observationHandler
    }
