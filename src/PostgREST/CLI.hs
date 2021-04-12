{-# LANGUAGE QuasiQuotes #-}
module PostgREST.CLI
  ( CLI (..)
  , Command (..)
  , readCLIShowHelp
  ) where

import qualified Options.Applicative as O
import qualified Protolude.Conv      as Conv

import Text.Heredoc (str)

import PostgREST.Version (prettyVersion)

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
readCLIShowHelp :: Bool -> IO CLI
readCLIShowHelp hasEnvironment =
  O.customExecParser prefs opts
  where
    prefs = O.prefs $ O.showHelpOnError <> O.showHelpOnEmpty
    opts = O.info parser $ O.fullDesc <> progDesc <> footer
    parser = O.helper <*> exampleParser <*> cliParser

    progDesc =
      O.progDesc $
        "PostgREST "
        <> Conv.toS prettyVersion
        <> " / create a REST API to an existing Postgres database"

    footer =
      O.footer $
        "To run PostgREST, please pass the FILENAME argument"
        <> " or set PGRST_ environment variables."

    exampleParser =
      O.infoOption exampleConfigFile $
        O.long "example"
        <> O.short 'e'
        <> O.help "Show an example configuration file"

    cliParser :: O.Parser CLI
    cliParser =
      CLI
        <$> (dumpConfigFlag <|> dumpSchemaFlag)
        <*> optionalIf hasEnvironment configFileOption

    configFileOption =
      O.strArgument $
        O.metavar "FILENAME"
        <> O.help "Path to configuration file (optional with PGRST_ environment variables)"

    dumpConfigFlag =
      O.flag CmdRun CmdDumpConfig $
        O.long "dump-config"
        <> O.help "Dump loaded configuration and exit"

    dumpSchemaFlag =
      O.flag CmdRun CmdDumpSchema $
        O.long "dump-schema"
        <> O.help "Dump loaded schema as JSON and exit (for debugging, output structure is unstable)"

    optionalIf :: Alternative f => Bool -> f a -> f (Maybe a)
    optionalIf True  = O.optional
    optionalIf False = fmap Just

exampleConfigFile :: [Char]
exampleConfigFile =
  [str|### REQUIRED:
      |db-uri = "postgres://user:pass@localhost:5432/dbname"
      |db-schema = "public"
      |db-anon-role = "postgres"
      |
      |### OPTIONAL:
      |## number of open connections in the pool
      |db-pool = 10
      |
      |## Time to live, in seconds, for an idle database pool connection.
      |db-pool-timeout = 10
      |
      |## extra schemas to add to the search_path of every request
      |db-extra-search-path = "public"
      |
      |## limit rows in response
      |# db-max-rows = 1000
      |
      |## stored proc to exec immediately after auth
      |# db-pre-request = "stored_proc_name"
      |
      |## stored proc that overrides the root "/" spec
      |## it must be inside the db-schema
      |# db-root-spec = "stored_proc_name"
      |
      |## Notification channel for reloading the schema cache
      |db-channel = "pgrst"
      |
      |## Enable or disable the notification channel
      |db-channel-enabled = false
      |
      |## Enable in-database configuration
      |db-config = true
      |
      |## how to terminate database transactions
      |## possible values are:
      |## commit (default)
      |##   transaction is always committed, this can not be overriden
      |## commit-allow-override
      |##   transaction is committed, but can be overriden with Prefer tx=rollback header
      |## rollback
      |##   transaction is always rolled back, this can not be overriden
      |## rollback-allow-override
      |##   transaction is rolled back, but can be overriden with Prefer tx=commit header
      |db-tx-end = "commit"
      |
      |## enable or disable prepared statements. disabling is only necessary when behind a connection pooler.
      |## when disabled, statements will be parametrized but won't be prepared.
      |db-prepared-statements = true
      |
      |server-host = "!4"
      |server-port = 3000
      |
      |## unix socket location
      |## if specified it takes precedence over server-port
      |# server-unix-socket = "/tmp/pgrst.sock"
      |
      |## unix socket file mode
      |## when none is provided, 660 is applied by default
      |# server-unix-socket-mode = "660"
      |
      |## base url for swagger output
      |openapi-server-proxy-uri = ""
      |
      |## choose a secret, JSON Web Key (or set) to enable JWT auth
      |## (use "@filename" to load from separate file)
      |# jwt-secret = "secret_with_at_least_32_characters"
      |# jwt-aud = "your_audience_claim"
      |jwt-secret-is-base64 = false
      |
      |## jspath to the role claim key
      |jwt-role-claim-key = ".role"
      |
      |## content types to produce raw output
      |# raw-media-types="image/png, image/jpg"
      |
      |## logging level, the admitted values are: crit, error, warn and info.
      |log-level = "error"
      |]
