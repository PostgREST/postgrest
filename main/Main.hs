{-# LANGUAGE CPP #-}

module Main (main) where

import System.IO (BufferMode (..), hSetBuffering)

import qualified PostgREST.App as App
import qualified PostgREST.CLI as CLI

import Protolude

#ifndef mingw32_HOST_OS
import qualified PostgREST.Unix as Unix
#endif

main :: IO ()
main = do
  setBuffering
  opts <- CLI.readCLIShowHelp
  CLI.main installSignalHandlers opts

installSignalHandlers :: App.SignalHandlerInstaller
#ifndef mingw32_HOST_OS
installSignalHandlers = Unix.installSignalHandlers
#else
installSignalHandlers _ = pass
#endif

setBuffering :: IO ()
setBuffering = do
  -- LineBuffering: the entire output buffer is flushed whenever a newline is
  -- output, the buffer overflows, a hFlush is issued or the handle is closed
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  hSetBuffering stderr LineBuffering
