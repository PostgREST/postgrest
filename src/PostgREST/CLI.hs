{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.CLI
  ( main
  , CLIOpts (..)
  , Commands (..)
  , Command (..)
  , readCLIShowHelp
  ) where

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Hasql.Transaction.Sessions as SQL
import qualified Options.Applicative        as O

import Data.Text.IO (hPutStrLn)
import Text.Heredoc (str)

import PostgREST.AppState    (AppState)
import PostgREST.Config      (AppConfig (..))
import PostgREST.SchemaCache (querySchemaCache)
import PostgREST.Version     (prettyVersion)

import qualified PostgREST.App      as App
import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config
import qualified PostgREST.Logger   as Logger

import Protolude hiding (hPutStrLn)


main :: CLIOpts -> IO ()
main CLIOpts{example, command=Commands{..}} = do
  conf@AppConfig{..} <-
    either panic identity <$> Config.readAppConfig mempty cliPath Nothing mempty mempty

  loggerState <- Logger.init
  -- Per https://github.com/PostgREST/postgrest/issues/268, we want to
  -- explicitly close the connections to PostgreSQL on shutdown.
  -- 'AppState.destroy' takes care of that.
  bracket
    (AppState.init conf $ Logger.logObservation loggerState)
    AppState.destroy
    (\appState -> case (example, cliCommand) of
      (_, CmdDumpConfig) -> do
        when configDbConfig $ AppState.reReadConfig True appState (const $ pure ())
        putStr . Config.toText =<< AppState.getConfig appState
      (_, CmdDumpSchemaCache) -> putStrLn =<< dumpSchema appState
      ("file", CmdRun)        -> putStrLn $ exampleConfig "file"
      ("db", CmdRun)          -> putStrLn $ exampleConfig "db"
      ("env", CmdRun)         -> putStrLn $ exampleConfig "env"
      (_, CmdRun)     -> App.run appState (Logger.logObservation loggerState))

-- | Dump SchemaCache schema to JSON
dumpSchema :: AppState -> IO LBS.ByteString
dumpSchema appState = do
  conf@AppConfig{..} <- AppState.getConfig appState
  result <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    AppState.usePool appState conf
      (transaction SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
      (const $ pure ())
  case result of
    Left e -> do
      hPutStrLn stderr $ "An error ocurred when loading the schema cache:\n" <> show e
      exitFailure
    Right sCache -> return $ JSON.encode sCache


-- | Command line interface options
data CLIOpts = CLIOpts
  { example :: Example
  , command :: Commands
  }

type Example = [Char]

data Commands = Commands
  { cliCommand :: Command
  , cliPath    :: Maybe FilePath
  }

data Command
  = CmdRun
  | CmdDumpConfig
  | CmdDumpSchemaCache

-- | Read command line interface options. Also prints help.
readCLIShowHelp :: IO CLIOpts
readCLIShowHelp =
  O.customExecParser prefs opts
  where
    prefs = O.prefs $ O.showHelpOnError <> O.showHelpOnEmpty
    opts = O.info parser $ O.fullDesc <> progDesc
    parser = O.helper <*> versionFlag <*> cliParser

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

    showExample :: O.Parser Example
    showExample =
      O.strOption $
          O.long "example"
          <> O.short 'e'
          <> O.metavar "EXAMPLETYPE"
          <> O.help "Type of config file/db/env"

    cliParser :: O.Parser CLIOpts
    cliParser =
      CLIOpts
        <$> showExample
        <*> (dumpConfigFlag <|> dumpSchemaCacheFlag)

    configFileOption =
      O.strArgument $
        O.metavar "FILENAME"
        <> O.help "Path to configuration file"

    dumpConfigFlag =
      Commands <$> (O.flag CmdRun CmdDumpConfig $
        O.long "dump-config"
        <> O.help "Dump loaded configuration and exit")
        <*> O.optional configFileOption

    dumpSchemaCacheFlag =
      Commands <$> (O.flag CmdRun CmdDumpSchemaCache $
        O.long "dump-schema-cache"
        <> O.help "Dump loaded schema as JSON and exit (for debugging, output structure is unstable)")
        <*> O.optional configFileOption

-- Shown in CLI "--example file"
exampleConfig :: [Char] -> [Char]
exampleConfig "file" =
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
      |## Logging level, the admitted values are: crit, error, warn and info.
      |log-level = "error"
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
exampleConfig "db" = -- Shown in CLI "--example db"
  [str|## reloadable config options
      |ALTER ROLE db_config_authenticator SET pgrst.admin_server_port = '3001';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_anon_role = 'anon';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_channel = 'pgrst';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_channel_enabled = 'true';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_config = 'true';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_pre_config = 'postgrest.pre_config';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_extra_search_path = 'public';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_max_rows = '1000';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_plan_enabled = 'false';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_pool = '10';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_pool_acquisition_timeout = '10';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_pool_max_lifetime = '1800';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_pool_max_idletime = '30';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_pool_automatic_recovery = 'true';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_pre_request = 'stored_proc_name';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_prepared_statements = 'true';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_schemas = 'public';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_tx_end = 'commit';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.db_uri = 'postgresql://';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.jwt_aut = 'your_audience_claim';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.jwt_role_claim_key = '.role';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.jwt_secret_is_base64 = 'false';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.jwt_cache_max_lifetime = '0';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.log_level = 'error';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.openapi_mode = 'follow-privileges';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.openapi_server_proxy_uri = '';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.server_cors_allowed_origins = '';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.server_host = '!4';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.server_port = '3000';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.server_timing_enabled = 'false';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.server_unix_socket = '/tmp/pgrst.sock';
      |
      |ALTER ROLE db_config_authenticator SET pgrst.server_unix_socket_mods = '660';
      |]
exampleConfig "env" = -- Shown in CLI "--example env"
  [str|## example env config
      |PGRST_APP_SETTINGS_test2: test
      |
      |PGRST_APP_SETTINGS_test: test
      |
      |PGRST_DB_AGGREGATES_ENABLED: true
      |
      |PGRST_DB_ANON_ROLE: root
      |
      |PGRST_DB_CHANNEL: postgrest
      |
      |PGRST_DB_CHANNEL_ENABLED: false
      |
      |PGRST_DB_EXTRA_SEARCH_PATH: public, test
      |
      |PGRST_DB_MAX_ROWS: 1000
      |
      |PGRST_DB_PLAN_ENABLED: true
      |
      |PGRST_DB_POOL: 1
      |
      |PGRST_DB_POOL_ACQUISITION_TIMEOUT: 30
      |
      |PGRST_DB_POOL_MAX_LIFETIME: 3600
      |
      |PGRST_DB_POOL_MAX_IDLETIME: 60
      |
      |PGRST_DB_POOL_AUTOMATIC_RECOVERY: false
      |
      |PGRST_DB_PREPARED_STATEMENTS: false
      |
      |PGRST_DB_PRE_REQUEST: please_run_fast
      |
      |PGRST_DB_ROOT_SPEC: openapi_v3
      |
      |PGRST_DB_SCHEMAS: multi,   tenant,setup
      |
      |PGRST_DB_CONFIG: false
      |
      |PGRST_DB_PRE_CONFIG: "postgrest.pre_config"
      |
      |PGRST_DB_TX_END: rollback-allow-override
      |
      |PGRST_DB_URI: tmp_db
      |
      |PGRST_DB_USE_LEGACY_GUCS: false
      |
      |PGRST_JWT_AUD: 'https://postgrest.org'
      |
      |PGRST_JWT_ROLE_CLAIM_KEY: '.user[0]."real-role"'
      |
      |PGRST_JWT_SECRET: c2VjdXJpdHl0aHJvdWdob2JzY3VyaXR5
      |
      |PGRST_JWT_SECRET_IS_BASE64: true
      |
      |PGRST_JWT_CACHE_MAX_LIFETIME: 86400
      |
      |PGRST_LOG_LEVEL: info
      |
      |PGRST_OPENAPI_MODE: 'ignore-privileges'
      |
      |PGRST_OPENAPI_SECURITY_ACTIVE: true
      |
      |PGRST_OPENAPI_SERVER_PROXY_URI: 'https://postgrest.org'
      |
      |PGRST_SERVER_CORS_ALLOWED_ORIGINS: "http://example.com"
      |
      |PGRST_SERVER_HOST: 0.0.0.0
      |
      |PGRST_SERVER_PORT: 80
      |
      |PGRST_SERVER_TRACE_HEADER: X-Request-Id
      |
      |PGRST_SERVER_TIMING_ENABLED: true
      |
      |PGRST_SERVER_UNIX_SOCKET: /tmp/pgrst_io_test.sock
      |
      |PGRST_SERVER_UNIX_SOCKET_MODE: 777
      |
      |PGRST_ADMIN_SERVER_PORT: 3001
      |]
exampleConfig _ = "print error" -- unreachable
