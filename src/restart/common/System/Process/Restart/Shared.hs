{-# LANGUAGE RankNTypes #-}

module System.Process.Restart.Shared
  ( ProcessConfig(..)
  , HandoverError(..)
  , StartupMode(..)
  , AppRun
  , Ready
  , isStandalone
  , isReplacement
  , currentProcessConfig
  ) where

import Data.String        (String)
import System.Environment (getEnvironment, getExecutablePath)

import Protolude

-- | Opaque process configuration used to start replacement processes.
data ProcessConfig = ProcessConfig
  { processExecutable :: FilePath
  , processArguments  :: [String]
  , processEnv        :: [(String, String)]
  } deriving (Eq, Show)

newtype HandoverError
  = HandoverProtocolError ByteString
  deriving (Show)

instance Exception HandoverError

-- | Startup mode detected by the restart runner.
data StartupMode
  = StartupStandalone
  | StartupReplacement
  deriving (Eq, Show)

-- | Application entry point executed by the restart runner.
type AppRun res =
  StartupMode ->
  -- ^ Startup mode detected by the runner.
  Ready ->
  -- ^ Action to mark the application as ready for serving.
  IO res

-- | Marks the application as ready and exposes replacement requests.
type Ready =
  forall a b.
  IO a ->
  -- ^ Action to stop the running application.
  (IO a -> IO b) ->
  -- ^ Action to register process replacement requests.
  IO (Maybe b)

isStandalone :: StartupMode -> Bool
isStandalone =
  (== StartupStandalone)

isReplacement :: StartupMode -> Bool
isReplacement =
  (== StartupReplacement)

-- | Build a process config for re-executing the current program with the
-- current command-line arguments and environment.
currentProcessConfig :: IO ProcessConfig
currentProcessConfig =
  ProcessConfig
    <$> getExecutablePath
    <*> getArgs
    <*> getEnvironment
