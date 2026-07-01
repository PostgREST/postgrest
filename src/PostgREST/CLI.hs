{-# LANGUAGE NamedFieldPuns  #-}
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
import           System.IO.Error            (isDoesNotExistError)

import PostgREST.AppState    (AppState)
import PostgREST.Config      (AppConfig (..))
import PostgREST.Observation (Observation (..))
import PostgREST.SchemaCache (SchemaCache, querySchemaCache)
import PostgREST.Version     (prettyVersion)

import qualified PostgREST.App      as App
import qualified PostgREST.AppState as AppState
import qualified PostgREST.Client   as Client
import qualified PostgREST.Config   as Config

import Protolude


main :: CLI -> IO ()
main CLI{cliCommand, cliPath, cliSchemaCachePath} = do
  conf <-
    either panic identity <$> Config.readAppConfig mempty cliPath Nothing mempty mempty
  case cliCommand of
    Client adminCmd -> runClientCommand conf adminCmd
    Run runCmd      -> runAppCommand conf cliSchemaCachePath runCmd

-- | Run command using http-client to communicate with an already running postgrest
runClientCommand :: AppConfig -> ClientCommand -> IO ()
runClientCommand conf CmdReady = Client.ready conf

-- | Run postgrest with command
runAppCommand :: AppConfig -> Maybe FilePath -> RunCommand -> IO ()
runAppCommand conf@AppConfig{..} schemaCachePath runCmd = do
  initAppState <- case runCmd of
    CmdRun -> do
      initialSchemaCache <- join <$> traverse readSchemaCacheDump schemaCachePath
      pure $ maybe (AppState.init conf) (AppState.initWithSchemaCache conf) initialSchemaCache
    _ ->
      pure $ AppState.init conf

  -- Per https://github.com/PostgREST/postgrest/issues/268, we want to
  -- explicitly close the connections to PostgreSQL on shutdown.
  -- 'AppState.destroy' takes care of that.
  bracket
    initAppState
    AppState.destroy
    (\appState -> case runCmd of
      CmdDumpConfig -> do
        when configDbConfig $ AppState.readInDbConfig True appState
        putStr . Config.toText =<< AppState.getConfig appState
      CmdDumpSchema -> do
        when configDbConfig $ AppState.readInDbConfig True appState
        putStrLn =<< dumpSchema appState
      CmdRun ->
        App.run appState)

-- | Dump SchemaCache schema to JSON
dumpSchema :: AppState -> IO LBS.ByteString
dumpSchema appState = do
  conf@AppConfig{..} <- AppState.getConfig appState
  result <-
    AppState.usePool appState (SQL.transactionNoRetry SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
  case result of
    Left e -> do
      let observer = AppState.getObserver appState
      observer $ SchemaCacheErrorObs configDbSchemas configDbExtraSearchPath e
      exitFailure
    Right (sCache, _) -> return $ JSON.encode sCache

readSchemaCacheDump :: FilePath -> IO (Maybe SchemaCache)
readSchemaCacheDump path =
  fmap decodeSchemaCache <$> catch (Just <$> LBS.readFile path) handleReadError
  where
    decodeSchemaCache =
      fromRight (panic $ "Error loading schema cache dump from " <> toS path) . JSON.eitherDecode

    handleReadError :: IOException -> IO (Maybe LBS.ByteString)
    handleReadError e
      | isDoesNotExistError e = pure Nothing
      | otherwise             = throwIO e

-- | Command line interface options
data CLI = CLI
  { cliCommand         :: Command
  , cliPath            :: Maybe FilePath
  , cliSchemaCachePath :: Maybe FilePath
  }

data Command
  = Client ClientCommand
  | Run RunCommand

data ClientCommand
  = CmdReady

data RunCommand
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
      O.infoOption Config.exampleConfigFile $
        O.long "example"
        <> O.short 'e'
        <> O.help "Show an example configuration file"

    cliParser :: O.Parser CLI
    cliParser =
      CLI
        <$> (dumpConfigFlag <|> dumpSchemaFlag <|> readyFlag)
        <*> O.optional configFileOption
        <*> O.optional loadSchemaCacheOption

    configFileOption =
      O.strArgument $
        O.metavar "FILENAME"
        <> O.help "Path to configuration file"

    loadSchemaCacheOption =
      O.strOption $
        O.long "load-schema-cache"
        <> O.metavar "FILENAME"
        <> O.help "Load schema cache from JSON dump at startup if the file exists"

    dumpConfigFlag =
      O.flag (Run CmdRun) (Run CmdDumpConfig) $
        O.long "dump-config"
        <> O.help "Dump loaded configuration and exit"

    dumpSchemaFlag =
      O.flag (Run CmdRun) (Run CmdDumpSchema) $
        O.long "dump-schema"
        <> O.help "Dump loaded schema as JSON and exit (for debugging, output structure is unstable)"

    readyFlag =
      O.flag (Run CmdRun) (Client CmdReady) $
        O.long "ready"
        <> O.help "Checks the health of PostgREST by doing a request on the admin server /ready endpoint"
