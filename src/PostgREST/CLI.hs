{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.CLI
  ( main
  , CLI (..)
  , Command (..)
  , readCLIShowHelp
  ) where

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Hasql.Transaction.Sessions as SQL
import qualified Options.Applicative        as O

import Text.Heredoc (str)

import PostgREST.AppState    (AppState)
import PostgREST.Config      (AppConfig (..))
import PostgREST.Observation (Observation (..))
import PostgREST.SchemaCache (querySchemaCache)
import PostgREST.Version     (prettyVersion)

import qualified PostgREST.App      as App
import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config

import Protolude


main :: CLI -> IO ()
main CLI{cliCommand, cliPath} = do
  conf@AppConfig{..} <-
    either panic identity <$> Config.readAppConfig mempty cliPath Nothing mempty mempty

  -- Per https://github.com/PostgREST/postgrest/issues/268, we want to
  -- explicitly close the connections to PostgreSQL on shutdown.
  -- 'AppState.destroy' takes care of that.
  bracket
    (AppState.init conf)
    AppState.destroy
    (\appState -> case cliCommand of
      CmdDumpConfig -> do
        when configDbConfig $ AppState.readInDbConfig True appState
        putStr . Config.toText =<< AppState.getConfig appState
      CmdDumpSchema -> do
        when configDbConfig $ AppState.readInDbConfig True appState
        putStrLn =<< dumpSchema appState
      CmdRun -> App.run appState)

-- | Dump SchemaCache schema to JSON
dumpSchema :: AppState -> IO LBS.ByteString
dumpSchema appState = do
  conf@AppConfig{..} <- AppState.getConfig appState
  result <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    AppState.usePool appState
      (transaction SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
  case result of
    Left e -> do
      let observer = AppState.getObserver appState
      observer $ SchemaCacheErrorObs e
      exitFailure
    Right sCache -> return $ JSON.encode sCache

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
readCLIShowHelp :: IO CLI
readCLIShowHelp =
  O.customExecParser prefs opts
  where
    prefs = O.prefs $ O.showHelpOnError <> O.showHelpOnEmpty
    opts = O.info parser $ O.fullDesc <> progDesc
    parser = O.helper <*> versionFlag <*> exampleParser <*> cliParser

    progDesc =
      O.progDesc $
        "PostgREST "
        <> BS.unpack prettyVersion
        <> " / create a REST API to an existing Postgres database"

    versionFlag =
      O.infoOption ("PostgREST " <> BS.unpack prettyVersion) $
        O.long "version"
        <> O.short 'v'
        <> O.help "Show the version information"

    exampleParser =
      O.infoOption exampleConfigFile $
        O.long "example"
        <> O.short 'e'
        <> O.help "Show an example configuration file"

    cliParser :: O.Parser CLI
    cliParser =
      CLI
        <$> (dumpConfigFlag <|> dumpSchemaFlag)
        <*> O.optional configFileOption

    configFileOption =
      O.strArgument $
        O.metavar "FILENAME"
        <> O.help "Path to configuration file"

    dumpConfigFlag =
      O.flag CmdRun CmdDumpConfig $
        O.long "dump-config"
        <> O.help "Dump loaded configuration and exit"

    dumpSchemaFlag =
      O.flag CmdRun CmdDumpSchema $
        O.long "dump-schema"
        <> O.help "Dump loaded schema as JSON and exit (for debugging, output structure is unstable)"

exampleConfigFile :: [Char]
exampleConfigFile =
  [str|## Admin server used for checks. It's disabled by default unless a port is specified.
      |# admin-server-port = 3001
      |
      |## The database role to use when no client authentication is provided
      |# db-anon-role = "anon"
      |
      |## Notification channel for reloading the schema cache
      |db-channel = "pgrst"
      |
      |## Enable or disable the notification channel
      |db-channel-enabled = true
      |
      |## Enable in-database configuration
      |db-config = true
      |
      |## Function for in-database configuration
      |## db-pre-config = "postgrest.pre_config"
      |
      |## Extra schemas to add to the search_path of every request
      |db-extra-search-path = "public"
      |
      |## Limit rows in response
      |# db-max-rows = 1000
      |
      |## Allow getting the EXPLAIN plan through the `Accept: application/vnd.pgrst.plan` header
      |# db-plan-enabled = false
      |
      |## Number of open connections in the pool
      |db-pool = 10
      |
      |## Time in seconds to wait to acquire a slot from the connection pool
      |# db-pool-acquisition-timeout = 10
      |
      |## Time in seconds after which to recycle pool connections
      |# db-pool-max-lifetime = 1800
      |
      |## Time in seconds after which to recycle unused pool connections
      |# db-pool-max-idletime = 30
      |
      |## Allow automatic database connection retrying
      |# db-pool-automatic-recovery = true
      |
      |## Stored proc to exec immediately after auth
      |# db-pre-request = "stored_proc_name"
      |
      |## Enable or disable prepared statements. disabling is only necessary when behind a connection pooler.
      |## When disabled, statements will be parametrized but won't be prepared.
      |db-prepared-statements = true
      |
      |## The name of which database schema to expose to REST clients
      |db-schemas = "public"
      |
      |## How to terminate database transactions
      |## Possible values are:
      |## commit (default)
      |##   Transaction is always committed, this can not be overriden
      |## commit-allow-override
      |##   Transaction is committed, but can be overriden with Prefer tx=rollback header
      |## rollback
      |##   Transaction is always rolled back, this can not be overriden
      |## rollback-allow-override
      |##   Transaction is rolled back, but can be overriden with Prefer tx=commit header
      |db-tx-end = "commit"
      |
      |## The standard connection URI format, documented at
      |## https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
      |db-uri = "postgresql://"
      |
      |# jwt-aud = "your_audience_claim"
      |
      |## Jspath to the role claim key
      |jwt-role-claim-key = ".role"
      |
      |## Choose a secret, JSON Web Key (or set) to enable JWT auth
      |## (use "@filename" to load from separate file)
      |# jwt-secret = "secret_with_at_least_32_characters"
      |jwt-secret-is-base64 = false
      |
      |## Enables and set JWT Cache max lifetime, disables caching with 0
      |# jwt-cache-max-lifetime = 0
      |
      |## Logging level, the admitted values are: crit, error, warn, info and debug.
      |log-level = "error"
      |
      |## Log the requested SQL query at the current log-level.
      |log-query = "disabled"
      |
      |## Determine if the OpenAPI output should follow or ignore role privileges or be disabled entirely.
      |## Admitted values: follow-privileges, ignore-privileges, disabled
      |openapi-mode = "follow-privileges"
      |
      |## Base url for the OpenAPI output
      |openapi-server-proxy-uri = ""
      |
      |## Configurable CORS origins
      |# server-cors-allowed-origins = ""
      |
      |server-host = "!4"
      |server-port = 3000
      |
      |## Allow getting the request-response timing information through the `Server-Timing` header
      |server-timing-enabled = false
      |
      |## Unix socket location
      |## if specified it takes precedence over server-port
      |# server-unix-socket = "/tmp/pgrst.sock"
      |
      |## Unix socket file mode
      |## When none is provided, 660 is applied by default
      |# server-unix-socket-mode = "660"
      |]
