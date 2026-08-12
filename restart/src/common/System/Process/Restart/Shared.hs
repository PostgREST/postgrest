{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module System.Process.Restart.Shared
  ( ProcessConfig(..)
  , HandoverError(..)
  , StartupMode(..)
  , RestartSignal(..)
  , SignalAction(..)
  , SignalHandler
  , SignalHandlers(..)
  , handleSignal
  , AppRun
  , Ready
  , isStandalone
  , isReplacement
  , currentProcessConfig
  ) where

import qualified Data.Map.Strict    as Map
import           Data.String        (String)
import           Foreign.C.Types    (CInt)
import           System.Environment (getEnvironment, getExecutablePath)

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

-- | POSIX signal used by the restart runner.
newtype RestartSignal = RestartSignal CInt
  deriving (Eq, Ord, Show)

-- | Lifecycle action performed when a configured signal is received.
data SignalAction
  = Stop
  | Restart
  | None
  deriving (Eq, Show)

-- | Signal callback that can observe the signal and choose a lifecycle action.
type SignalHandler =
  IO SignalAction

-- | Static signal handling policy.
newtype SignalHandlers = SignalHandlers
  { signalHandlersMap :: Map RestartSignal SignalHandler
  -- ^ Mapping from signals to signal callbacks.
  }
  deriving newtype (Semigroup, Monoid)

handleSignal :: RestartSignal -> SignalHandler -> SignalHandlers
handleSignal signal handler =
  SignalHandlers $ Map.singleton signal handler

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
