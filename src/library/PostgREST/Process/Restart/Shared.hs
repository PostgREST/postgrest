{-# LANGUAGE RankNTypes #-}

module PostgREST.Process.Restart.Shared
  ( ReplacementConfig(..)
  , HandoverError(..)
  , HandoverMode(..)
  , AppRun
  , Ready
  , isStandalone
  , currentReplacementConfig
  ) where

import Data.String        (String)
import System.Environment (getEnvironment, getExecutablePath)

import Protolude

-- | Opaque replacement process configuration.
data ReplacementConfig = ReplacementConfig
  { replacementExecutable :: FilePath
  , replacementArguments  :: [String]
  , replacementEnv        :: [(String, String)]
  } deriving (Eq, Show)

data HandoverError
  = HandoverProtocolError ByteString
  | HandoverFailed ByteString
  deriving (Show)

instance Exception HandoverError

-- | Process mode detected by the handover runner.
newtype HandoverMode = HandoverMode {
    isReplacement :: Bool
  }
  deriving (Eq, Show)

type AppRun res = HandoverMode -> Ready -> IO res

type Ready = forall a b. IO a -> (IO a -> IO b) -> IO b

isStandalone :: HandoverMode -> Bool
isStandalone = not . isReplacement

-- | Build a replacement config for re-executing the current program with the
-- current command-line arguments and environment.
currentReplacementConfig :: IO ReplacementConfig
currentReplacementConfig =
  ReplacementConfig
    <$> getExecutablePath
    <*> getArgs
    <*> getEnvironment
