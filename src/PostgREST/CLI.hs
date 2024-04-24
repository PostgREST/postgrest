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

import Data.Text.IO (hPutStrLn)
import Text.Heredoc (str)

import PostgREST.AppState    (AppState)
import PostgREST.Config      (AppConfig (..))
import PostgREST.SchemaCache (querySchemaCache)
import PostgREST.Version     (prettyVersion)

import qualified PostgREST.App      as App
import qualified PostgREST.AppState as AppState
import qualified PostgREST.Config   as Config

import Protolude hiding (hPutStrLn)


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
        when configDbConfig $ AppState.reReadConfig True appState
        putStr . Config.toText =<< AppState.getConfig appState
      CmdDumpSchemaCache -> putStrLn =<< dumpSchema appState
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
      hPutStrLn stderr $ "An error ocurred when loading the schema cache:\n" <> show e
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
  | CmdDumpSchemaCache

data Example = ExampleFile | ExampleDb | ExampleEnv

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

    exampleParser = exampleParserFile <|> exampleParserDb <|> exampleParserEnv

    exampleParserFile =
      O.infoOption (exampleConfig ExampleFile) $
        O.long "example-file"
        <> O.short 'e'
        <> O.help "Show an example of a configuration file"

    exampleParserDb =
      O.infoOption (exampleConfig ExampleDb) $
        O.long "example-db"
        <> O.help "Show an example of in-database configuration"

    exampleParserEnv =
      O.infoOption (exampleConfig ExampleEnv) $
        O.long "example-env"
        <> O.help "Show an example of environment variables configuration"

    cliParser :: O.Parser CLI
    cliParser =
      CLI
        <$> (dumpConfigFlag <|> dumpSchemaCacheFlag)
        <*> O.optional configFileOption

    configFileOption =
      O.strArgument $
        O.metavar "FILENAME"
        <> O.help "Path to configuration file"

    dumpConfigFlag =
      O.flag CmdRun CmdDumpConfig $
        O.long "dump-config"
        <> O.help "Dump loaded configuration and exit"

    dumpSchemaCacheFlag =
      O.flag CmdRun CmdDumpSchemaCache $
        O.long "dump-schema-cache"
        <> O.help "Dump loaded schema as JSON and exit (for debugging, output structure is unstable)"

exampleConfig :: Example -> [Char]
exampleConfig ExampleFile =
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
      |
      |jwt-secret-is-base64 = false
      |
      |## Enables and set JWT Cache max lifetime, disables caching with 0
      |# jwt-cache-max-lifetime = 0
      |
      |## Logging level, the admitted values are: crit, error, warn, info and debug.
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
exampleConfig ExampleDb =
  [str|## Admin server used for checks. It's disabled by default unless a port is specified.
      |# ALTER ROLE authenticator SET pgrst.admin_server_port = '3001';
      |
      |## The database role to use when no client authentication is provided
      |# ALTER ROLE authenticator SET pgrst.db_anon_role = 'anon';
      |
      |## Notification channel for reloading the schema cache
      |ALTER ROLE authenticator SET pgrst.db_channel = 'pgrst';
      |
      |## Enable or disable the notification channel
      |ALTER ROLE authenticator SET pgrst.db_channel_enabled = 'true';
      |
      |## Enable in-database configuration
      |ALTER ROLE authenticator SET pgrst.db_config = 'true';
      |
      |## Function for in-database configuration
      |# ALTER ROLE authenticator SET pgrst.db_pre_config = 'postgrest.pre_config';
      |
      |## Extra schemas to add to the search_path of every request
      |ALTER ROLE authenticator SET pgrst.db_extra_search_path = 'public';
      |
      |## Limit rows in response
      |# ALTER ROLE authenticator SET pgrst.db_max_rows = '1000';
      |
      |## Allow getting the EXPLAIN plan through the `Accept: application/vnd.pgrst.plan` header
      |# ALTER ROLE authenticator SET pgrst.db_plan_enabled = 'false';
      |
      |## Number of open connections in the pool
      |ALTER ROLE authenticator SET pgrst.db_pool = '10';
      |
      |## Time in seconds to wait to acquire a slot from the connection pool
      |# ALTER ROLE authenticator SET pgrst.db_pool_acquisition_timeout = '10';
      |
      |## Time in seconds after which to recycle pool connections
      |# ALTER ROLE authenticator SET pgrst.db_pool_max_lifetime = '1800';
      |
      |## Time in seconds after which to recycle unused pool connections
      |# ALTER ROLE authenticator SET pgrst.db_pool_max_idletime = '30';
      |
      |## Allow automatic database connection retrying
      |# ALTER ROLE authenticator SET pgrst.db_pool_automatic_recovery = 'true';
      |
      |## Stored proc to exec immediately after auth
      |# ALTER ROLE authenticator SET pgrst.db_pre_request = 'stored_proc_name';
      |
      |## Enable or disable prepared statements. disabling is only necessary when behind a connection pooler.
      |## When disabled, statements will be parametrized but won't be prepared.
      |ALTER ROLE authenticator SET pgrst.db_prepared_statements = 'true';
      |
      |## The name of which database schema to expose to REST clients
      |ALTER ROLE authenticator SET pgrst.db_schemas = 'public';
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
      |ALTER ROLE authenticator SET pgrst.db_tx_end = 'commit';
      |
      |## The standard connection URI format, documented at
      |## https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
      |ALTER ROLE authenticator SET pgrst.db_uri = 'postgresql://';
      |
      |# ALTER ROLE authenticator SET pgrst.jwt_aut = 'your_audience_claim';
      |
      |## Jspath to the role claim key
      |ALTER ROLE authenticator SET pgrst.jwt_role_claim_key = '.role';
      |
      |## Choose a secret, JSON Web Key (or set) to enable JWT auth
      |## (use "@filename" to load from separate file)
      |# ALTER ROLE authenticator SET pgrst.jwt_secret = 'secret_with_at_least_32_characters'
      |
      |ALTER ROLE authenticator SET pgrst.jwt_secret_is_base64 = 'false';
      |
      |## Enables and set JWT Cache max lifetime, disables caching with 0
      |# ALTER ROLE authenticator SET pgrst.jwt_cache_max_lifetime = '0';
      |
      |## Logging level, the admitted values are: crit, error, warn and info.
      |ALTER ROLE authenticator SET pgrst.log_level = 'error';
      |
      |## Determine if the OpenAPI output should follow or ignore role privileges or be disabled entirely.
      |## Admitted values: follow-privileges, ignore-privileges, disabled
      |ALTER ROLE authenticator SET pgrst.openapi_mode = 'follow-privileges';
      |
      |## Base url for the OpenAPI output
      |ALTER ROLE authenticator SET pgrst.openapi_server_proxy_uri = '';
      |
      |## Configurable CORS origins
      |# ALTER ROLE authenticator SET pgrst.server_cors_allowed_origins = '';
      |
      |ALTER ROLE authenticator SET pgrst.server_host = '!4';
      |
      |ALTER ROLE authenticator SET pgrst.server_port = '3000';
      |
      |## Allow getting the request-response timing information through the `Server-Timing` header
      |ALTER ROLE authenticator SET pgrst.server_timing_enabled = 'false';
      |
      |## Unix socket location
      |## if specified it takes precedence over server-port
      |# ALTER ROLE authenticator SET pgrst.server_unix_socket = '/tmp/pgrst.sock';
      |
      |## Unix socket file mode
      |## When none is provided, 660 is applied by default
      |# ALTER ROLE authenticator SET pgrst.server_unix_socket_mods = '660';
      |]
exampleConfig ExampleEnv =
  [str|## Admin server used for checks. It's disabled by default unless a port is specified.
      |# export PGRST_ADMIN_SERVER_PORT=3001
      |
      |## The database role to use when no client authentication is provided
      |# export PGRST_DB_ANON_ROLE=root
      |
      |## Notification channel for reloading the schema cache
      |export PGRST_DB_CHANNEL=postgrest
      |
      |## Enable or disable the notification channel
      |export PGRST_DB_CHANNEL_ENABLED=false
      |
      |## Enable in-database configuration
      |export PGRST_DB_CONFIG=false
      |
      |## Function for in-database configuration
      |# export PGRST_DB_PRE_CONFIG='postgrest.pre_config'
      |
      |## Extra schemas to add to the search_path of every request
      |export PGRST_DB_EXTRA_SEARCH_PATH='public'
      |
      |## Limit rows in response
      |# export PGRST_DB_MAX_ROWS=1000
      |
      |## Allow getting the EXPLAIN plan through the `Accept: application/vnd.pgrst.plan` header
      |# export PGRST_DB_PLAN_ENABLED=false
      |
      |## Number of open connections in the pool
      |export PGRST_DB_POOL=10
      |
      |## Time in seconds to wait to acquire a slot from the connection pool
      |# export PGRST_DB_POOL_ACQUISITION_TIMEOUT=10
      |
      |## Time in seconds after which to recycle pool connections
      |# export PGRST_DB_POOL_MAX_LIFETIME=1800
      |
      |## Time in seconds after which to recycle unused pool connections
      |# export PGRST_DB_POOL_MAX_IDLETIME=30
      |
      |## Allow automatic database connection retrying
      |# export PGRST_DB_POOL_AUTOMATIC_RECOVERY=true
      |
      |## Stored proc to exec immediately after auth
      |# export PGRST_DB_PRE_REQUEST='stored_proc_name'
      |
      |## Enable or disable prepared statements. disabling is only necessary when behind a connection pooler.
      |## When disabled, statements will be parametrized but won't be prepared.
      |export PGRST_DB_PREPARED_STATEMENTS=true
      |
      |## The name of which database schema to expose to REST clients
      |export PGRST_DB_SCHEMAS='public'
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
      |export PGRST_DB_TX_END=commit
      |
      |## The standard connection URI format, documented at
      |## https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
      |export PGRST_DB_URI='postgresql://'
      |
      |# export PGRST_JWT_AUD='your_audience_claim'
      |
      |## Jspath to the role claim key
      |export PGRST_JWT_ROLE_CLAIM_KEY='.role'
      |
      |## Choose a secret, JSON Web Key (or set) to enable JWT auth
      |## (use "@filename" to load from separate file)
      |# export PGRST_JWT_SECRET='secret_with_at_least_32_characters'
      |
      |export PGRST_JWT_SECRET_IS_BASE64=false
      |
      |## Enables and set JWT Cache max lifetime, disables caching with 0
      |# export PGRST_JWT_CACHE_MAX_LIFETIME=0
      |
      |## Logging level, the admitted values are: crit, error, warn and info.
      |export PGRST_LOG_LEVEL=error
      |
      |## Determine if the OpenAPI output should follow or ignore role privileges or be disabled entirely.
      |## Admitted values: follow-privileges, ignore-privileges, disabled
      |export PGRST_OPENAPI_MODE='follow-privileges'
      |
      |## Base url for the OpenAPI output
      |export PGRST_OPENAPI_SERVER_PROXY_URI=''
      |
      |## Configurable CORS origins
      |# export PGRST_SERVER_CORS_ALLOWED_ORIGINS=''
      |
      |export PGRST_SERVER_HOST=!4
      |
      |export PGRST_SERVER_PORT=3000
      |
      |## Allow getting the request-response timing information through the `Server-Timing` header
      |export PGRST_SERVER_TIMING_ENABLED=false
      |
      |export PGRST_SERVER_TRACE_HEADER=X-Request-Id
      |
      |## Unix socket location
      |## if specified it takes precedence over server-port
      |# export PGRST_SERVER_UNIX_SOCKET='/tmp/pgrst.sock'
      |
      |## Unix socket file mode
      |## When none is provided, 660 is applied by default
      |# export PGRST_SERVER_UNIX_SOCKET_MODE=660
      |]
