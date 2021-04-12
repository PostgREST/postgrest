{-# LANGUAGE CPP #-}

module Main (main) where

import qualified Data.Map.Strict as M

import System.IO (BufferMode (..), hSetBuffering)

import qualified PostgREST.App as App
import qualified PostgREST.CLI as CLI

import PostgREST.Config (readPGRSTEnvironment)

import Protolude

#ifndef mingw32_HOST_OS
import qualified PostgREST.Unix as Unix
#endif

main :: IO ()
main = do
  setBuffering
  hasPGRSTEnv <- not . M.null <$> readPGRSTEnvironment
  opts <- CLI.readCLIShowHelp hasPGRSTEnv
  CLI.main installSignalHandlers runAppInSocket opts

installSignalHandlers :: App.SignalHandlerInstaller
#ifndef mingw32_HOST_OS
installSignalHandlers = Unix.installSignalHandlers
#else
installSignalHandlers _ = pass
#endif

runAppInSocket :: App.SocketRunner
#ifndef mingw32_HOST_OS
runAppInSocket = Unix.runAppInSocket
#else
runAppInSocket _ _ _ _ = pass
#endif

setBuffering :: IO ()
setBuffering = do
  -- LineBuffering: the entire output buffer is flushed whenever a newline is
  -- output, the buffer overflows, a hFlush is issued or the handle is closed
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  -- NoBuffering: output is written immediately and never stored in the buffer
  hSetBuffering stderr NoBuffering
