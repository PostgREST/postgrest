{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.CLI
  ( main
  , CLI (..)
  , Command (..)
  , readCLIShowHelp
  ) where

import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.KeyMap          as KM
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Hasql.Transaction.Sessions as SQL
import qualified Options.Applicative        as O

import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Types        (Action (..), DbAction (..),
                                          InvokeMethod (..), Resource (..))
import PostgREST.AppState                (AppState)
import PostgREST.Auth.Types              (AuthResult (..))
import PostgREST.Config                  (AppConfig (..))
import PostgREST.Error                   (ApiRequestError (..),
                                          Error (..))
import PostgREST.Logger                  (renderSnippet)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Observation             (Observation (..))
import PostgREST.Plan                    (actionPlan)
import PostgREST.Query                   (MainQuery (..), mainQuery)
import PostgREST.RangeQuery              (allRange)
import PostgREST.SchemaCache             (SchemaCache, querySchemaCache)
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..))
import PostgREST.Version                 (prettyVersion)

import qualified PostgREST.ApiRequest.Preferences as Preferences
import qualified PostgREST.ApiRequest.QueryParams as QueryParams
import qualified PostgREST.App                    as App
import qualified PostgREST.AppState               as AppState
import qualified PostgREST.Client                 as Client
import qualified PostgREST.Config                 as Config

import Protolude


main :: CLI -> IO ()
main CLI{cliCommand, cliPath} = do
  conf <-
    either panic identity <$> Config.readAppConfig mempty cliPath Nothing mempty mempty
  case cliCommand of
    Client adminCmd -> runClientCommand conf adminCmd
    Run runCmd      -> runAppCommand conf runCmd

-- | Run command using http-client to communicate with an already running postgrest
runClientCommand :: AppConfig -> ClientCommand -> IO ()
runClientCommand conf CmdReady = Client.ready conf

-- | Run postgrest with command
runAppCommand :: AppConfig -> RunCommand -> IO ()
runAppCommand conf@AppConfig{..} runCmd = do
  -- Per https://github.com/PostgREST/postgrest/issues/268, we want to
  -- explicitly close the connections to PostgreSQL on shutdown.
  -- 'AppState.destroy' takes care of that.
  bracket
    (AppState.init conf)
    AppState.destroy
    (\appState -> case runCmd of
      CmdDumpConfig -> do
        when configDbConfig $ AppState.readInDbConfig True appState
        putStr . Config.toText =<< AppState.getConfig appState
      CmdDumpSchema -> do
        when configDbConfig $ AppState.readInDbConfig True appState
        putStrLn =<< dumpSchema appState
      CmdQueryToSQL queryUrl -> do
        when configDbConfig $ AppState.readInDbConfig True appState
        queryToSQL appState queryUrl
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
      observer $ SchemaCacheErrorObs configDbSchemas configDbExtraSearchPath e
      exitFailure
    Right sCache -> return $ JSON.encode sCache

-- | Translate a PostgREST URL query to SQL
queryToSQL :: AppState -> Text -> IO ()
queryToSQL appState queryUrl = do
  conf@AppConfig{..} <- AppState.getConfig appState
  result <-
    let transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction in
    AppState.usePool appState
      (transaction SQL.ReadCommitted SQL.Read $ querySchemaCache conf)
  case result of
    Left e -> do
      let observer = AppState.getObserver appState
      observer $ SchemaCacheErrorObs configDbSchemas configDbExtraSearchPath e
      exitFailure
    Right sCache ->
      case translateQuery conf sCache queryUrl of
        Left err -> do
          hPutStrLn stderr $ ("Error: " :: Text) <> show err
          exitFailure
        Right sql -> BS.putStrLn sql

translateQuery :: AppConfig -> SchemaCache -> Text -> Either Error ByteString
translateQuery conf@AppConfig{..} sCache queryUrl = do
  let (path, qs) = T.breakOn "?" queryUrl
      queryString = if T.null qs then "" else encodeUtf8 $ T.drop 1 qs
      pathParts = filter (not . T.null) $ T.splitOn "/" path
      schema = NonEmpty.head configDbSchemas

  (resource, qi) <- case pathParts of
    ["rpc", name] -> Right (ResourceRoutine name, QualifiedIdentifier schema name)
    [name]        -> Right (ResourceRelation name, QualifiedIdentifier schema name)
    _             -> Left $ ApiRequestError InvalidResourcePath

  qPrms <- first (ApiRequestError . QueryParamError) $
           QueryParams.parse (isRoutine resource) queryString

  let act = case resource of
        ResourceRoutine _ -> ActDb $ ActRoutine qi (InvRead False)
        ResourceRelation _ -> ActDb $ ActRelationRead qi False
        ResourceSchema -> ActDb $ ActSchemaRead schema False

  let apiReq = ApiRequest
        { iAction = act
        , iRange = HM.empty
        , iTopLevelRange = allRange
        , iPayload = Nothing
        , iPreferences = defaultPreferences
        , iQueryParams = qPrms
        , iColumns = S.empty
        , iHeaders = []
        , iCookies = []
        , iPath = encodeUtf8 queryUrl
        , iMethod = "GET"
        , iSchema = schema
        , iNegotiatedByProfile = False
        , iAcceptMediaType = [MTApplicationJSON]
        , iContentMediaType = MTApplicationJSON
        }

  plan <- actionPlan act conf apiReq sCache

  let mainQ = mainQuery plan conf apiReq dummyAuthResult Nothing
  Right $ renderSnippet (mqMain mainQ)

  where
    isRoutine (ResourceRoutine _) = True
    isRoutine _ = False

    dummyAuthResult = AuthResult KM.empty ""

    defaultPreferences = Preferences.Preferences
      { Preferences.preferResolution = Nothing
      , Preferences.preferRepresentation = Nothing
      , Preferences.preferCount = Nothing
      , Preferences.preferTransaction = Nothing
      , Preferences.preferMissing = Nothing
      , Preferences.preferHandling = Nothing
      , Preferences.preferTimezone = Nothing
      , Preferences.preferMaxAffected = Nothing
      , Preferences.invalidPrefs = []
      }

-- | Command line interface options
data CLI = CLI
  { cliCommand :: Command
  , cliPath    :: Maybe FilePath
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
  | CmdQueryToSQL Text

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
        <$> (dumpConfigFlag <|> dumpSchemaFlag <|> queryToSQLOption <|> readyFlag)
        <*> O.optional configFileOption

    configFileOption =
      O.strArgument $
        O.metavar "FILENAME"
        <> O.help "Path to configuration file"

    dumpConfigFlag =
      O.flag (Run CmdRun) (Run CmdDumpConfig) $
        O.long "dump-config"
        <> O.help "Dump loaded configuration and exit"

    dumpSchemaFlag =
      O.flag (Run CmdRun) (Run CmdDumpSchema) $
        O.long "dump-schema"
        <> O.help "Dump loaded schema as JSON and exit (for debugging, output structure is unstable)"

    queryToSQLOption =
      Run . CmdQueryToSQL <$> O.strOption
        ( O.long "query-to-sql"
        <> O.metavar "QUERY"
        <> O.help "Translate a PostgREST URL query to SQL and exit (e.g., \"/items?select=id&id=gt.5\")"
        )

    readyFlag =
      O.flag (Run CmdRun) (Client CmdReady) $
        O.long "ready"
        <> O.help "Checks the health of PostgREST by doing a request on the admin server /ready endpoint"
