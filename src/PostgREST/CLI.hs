module PostgREST.CLI
  ( CLI (..)
  , Command (..)
  , readCLIShowHelp
  ) where

import qualified Data.Map.Strict     as M
import qualified Options.Applicative as O
import qualified Protolude.Conv      as Conv

import PostgREST.Config (Environment, example, prettyVersion)

import Protolude


-- | Command line interface options
data CLI = CLI
  { cliCommand :: Command
  , cliPath    :: Maybe FilePath
  }

data Command
  = CmdRun
  | CmdDumpConfig
  | CmdDumpSchema

-- | Read command line interface options. Also prints help.
readCLIShowHelp :: Environment -> IO CLI
readCLIShowHelp env = O.customExecParser parserPrefs opts
  where
    parserPrefs = O.prefs $ O.showHelpOnError <> O.showHelpOnEmpty

    opts = O.info (O.helper <*> exampleParser <*> cliParser) $
             O.fullDesc
             <> O.progDesc (
                 "PostgREST "
                 <> Conv.toS prettyVersion
                 <> " / create a REST API to an existing Postgres database"
               )
             <> O.footer "To run PostgREST, please pass the FILENAME argument or set PGRST_ environment variables."

    cliParser :: O.Parser CLI
    cliParser = CLI <$>
      (
        O.flag CmdRun CmdDumpConfig (
          O.long "dump-config" <>
          O.help "Dump loaded configuration and exit"
        )
        <|>
        O.flag CmdRun CmdDumpSchema (
          O.long "dump-schema" <>
          O.help "Dump loaded schema as JSON and exit (for debugging, output structure is unstable)"
        )
      )
      <*>
      optionalWithEnvironment (O.strArgument (
        O.metavar "FILENAME" <>
        O.help "Path to configuration file (optional with PGRST_ environment variables)"
      ))

    optionalWithEnvironment :: Alternative f => f a -> f (Maybe a)
    optionalWithEnvironment v
      | M.null env = Just <$> v
      | otherwise  = O.optional v

    exampleParser :: O.Parser (a -> a)
    exampleParser =
      O.infoOption example (
        O.long "example" <>
        O.short 'e' <>
        O.help "Show an example configuration file"
      )
