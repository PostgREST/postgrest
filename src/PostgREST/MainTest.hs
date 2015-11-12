module Main where


import           PostgREST.App
import           PostgREST.Config                     (AppConfig (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)
import           PostgREST.Error                      (errResponse, PgError)
import           PostgREST.Middleware
import           PostgREST.DbStructure
import           PostgREST.Types

import           Control.Monad                        (unless)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (encode)
import           Data.Functor.Identity
import           Data.Monoid                          ((<>))
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import qualified Hasql                                as H
import qualified Hasql.Postgres                       as P
import           Network.Wai
import           Network.Wai.Handler.Warp             hiding (Connection)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.IO                            (BufferMode (..),
                                                       hSetBuffering, stderr,
                                                       stdin, stdout)
-- import Data.Maybe (mapMaybe)
-- import Data.List (subsequences)
-- import Control.Monad (join)
-- import PostgREST.QueryBuilder
-- import           GHC.Exts              (groupWith)


isServerVersionSupported :: H.Session P.Postgres IO Bool
isServerVersionSupported = do
  Identity (row :: Text) <- H.tx Nothing $ H.singleEx [H.stmt|SHOW server_version_num|]
  return $ read (cs row) >= minimumPgVersion

hasqlError :: PgError -> IO a
hasqlError = error . cs . encode


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  -- let dbString = "postgres://postgrest_test@localhost:5432/postgrest_test" :: String
  --     conf = AppConfig dbString 3000 "postgrest_anonymous" "test" False "safe" 10 :: AppConfig

  conf <- readOptions
  let port = configPort conf

  unless ("secret" /= configJwtSecret conf) $
    putStrLn "WARNING, running in insecure mode, JWT secret is the default value"
  Prelude.putStrLn $ "Listening on port " ++
    (show $ configPort conf :: String)

  let pgSettings = P.StringSettings $ cs (configDatabase conf)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings
      middle = logStdout . defaultMiddle

  poolSettings <- maybe (fail "Improper session settings") return $
    H.poolSettings (fromIntegral $ configPool conf) 30
  pool :: H.Pool P.Postgres <- H.acquirePool pgSettings poolSettings

  supportedOrError <- H.session pool isServerVersionSupported
  either hasqlError
    (\supported ->
      unless supported $
        error (
          "Cannot run in this PostgreSQL version, PostgREST needs at least "
          <> show minimumPgVersion)
    ) supportedOrError

  -- what was this code for?
  -- roleOrError <- H.session pool $ do
  --   Identity (role :: Text) <- H.tx Nothing $ H.singleEx
  --     [H.stmt|SELECT SESSION_USER|]
  --   return role
  -- authenticator <- either hasqlError return roleOrError

  let txSettings = Just (H.ReadCommitted, Just True)
  metadata <- H.session pool $ H.tx txSettings $ do
    tabs <- allTables
    rels <- allRelations
    cols <- allColumns rels
    keys <- allPrimaryKeys
    return (tabs, rels, cols, keys)


  db <- either hasqlError
    (\(tabs, rels, cols, keys) ->

      return DbStructure {
          dbTables=tabs
        , dbColumns=cols
        , dbRelations=rels
        , dbPrimaryKeys=keys
        }
    ) metadata
  runSettings appSettings $ middle $ \ req respond -> do
    body <- strictRequestBody req
    resOrError <- liftIO $ H.session pool $ H.tx txSettings $
      runWithClaims conf (app db conf body) req
    either (respond . errResponse) respond resOrError

  --let allRels = relations db
      -- links = join $ map (combinations 2) $ filter ((>=1).length) $ groupWith groupFn $ filter ( (==Child). relType) allRels
      -- combinations k ns = filter ((k==).length) (subsequences ns)

  --print $ findRelation allRels "test" "projects" "users"
  --mapM_ print $ mapMaybe link2Relation links

  -- where
  --   groupFn :: Relation -> Text
  --   groupFn (Relation{relSchema=s, relTable=t}) = s<>"_"<>t
  --   link2Relation [
  --     Relation{relSchema=sc, relTable=lt, relColumns=lc1, relFTable=t, relFColumns=c},
  --     Relation{                           relColumns=lc2, relFTable=ft, relFColumns=fc}
  --     ]
  --     | lc1 /= lc2 && length lc1 == 1 && length lc2 == 1 = Just $ Relation sc t c ft fc Many (Just lt) (Just lc1) (Just lc2)
  --     | otherwise = Nothing
  --   link2Relation _ = Nothing
