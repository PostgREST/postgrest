{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Main where

import Paths_postgrest (version)
-- added
import PostgREST.PgStructure
import Data.Aeson
import Data.List (find)
import Data.Maybe (isJust)
import PostgREST.Types
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.Wai -- (strictRequestBody, pathInfo, requestMethod, requestHeaders)


import PostgREST.App
import PostgREST.Middleware
import PostgREST.Error(errResponse)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.List (intercalate)
import Data.Version (versionBranch)
import Data.Functor.Identity
import Data.Text(Text)
import qualified Hasql as H
import qualified Hasql.Postgres as P
import Options.Applicative hiding (columns)

import System.IO (stderr, stdin, stdout, hSetBuffering, BufferMode(..))

import PostgREST.Config (AppConfig(..), argParser)

isServerVersionSupported = do
  Identity (row :: Text) <- H.tx Nothing $ H.singleEx $ [H.stmt|SHOW server_version_num|]
  return $ read (cs row) >= 90200

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  let opts = info (helper <*> argParser) $
               fullDesc
               <> progDesc (
                 "PostgREST "
                 <> prettyVersion
                 <> " / create a REST API to an existing Postgres database"
               )
      parserPrefs = prefs showHelpOnError
  conf <- customExecParser parserPrefs opts
  let port = configPort conf

  unless (configSecure conf) $
    putStrLn "WARNING, running in insecure mode, auth will be in plaintext"
  unless ("secret" /= configJwtSecret conf) $
    putStrLn "WARNING, running in insecure mode, JWT secret is the default value"
  Prelude.putStrLn $ "Listening on port " ++
    (show $ configPort conf :: String)

  let pgSettings = P.ParamSettings (cs $ configDbHost conf)
                     (fromIntegral $ configDbPort conf)
                     (cs $ configDbUser conf)
                     (cs $ configDbPass conf)
                     (cs $ configDbName conf)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings
      middle = logStdout . defaultMiddle (configSecure conf)

  poolSettings <- maybe (fail "Improper session settings") return $
    H.poolSettings (fromIntegral $ configPool conf) 30
  pool :: H.Pool P.Postgres <- H.acquirePool pgSettings poolSettings

  resOrError <- H.session pool isServerVersionSupported
  either (fail . show)
    (\supported ->
      unless supported $
        fail "Cannot run in this PostgreSQL version, PostgREST needs at least 9.2.0"
    ) resOrError

    -- read the structure of the database
    -- read the structure of the database
  let txParam = (Just (H.ReadCommitted, Just True))

  tblsRes <-  H.session pool $ H.tx txParam alltables
  let allTables = either (fail . show) id tblsRes

  relsRes <-  H.session pool $ H.tx txParam allrelations
  let allRelations = either (fail . show) id relsRes

  colsRes <-  H.session pool $ H.tx txParam $ allcolumns allRelations
  let allColumns = either (fail . show) id colsRes

  pkRes <-  H.session pool $ H.tx txParam $ allprimaryKeys
  let allPrimaryKeys = either (fail . show) id pkRes

  -- tableAclRes <-  H.session pool $ H.tx txParam $ alltablesAcl
  -- let allTablesAcl = either (fail . show) id tableAclRes


  let dbstructure = DbStructure {
      tables=allTables
    , columns=allColumns
    , relations=allRelations
    , primaryKeys=allPrimaryKeys
    --, tablesAcl=allTablesAcl
    }

  runSettings appSettings $ middle $ \ req respond -> do
    body <- strictRequestBody req
    resOrError <- liftIO $ H.session pool $ H.tx (Just (H.ReadCommitted, Just True)) $
      authenticated conf (app dbstructure conf body) req
    either (respond . errResponse) respond resOrError

  where
    prettyVersion = intercalate "." $ map show $ versionBranch version
