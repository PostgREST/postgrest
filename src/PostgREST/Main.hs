module Main where


import           PostgREST.App
import           PostgREST.Config                     (AppConfig (..),
                                                       minimumPgVersion,
                                                       prettyVersion,
                                                       readOptions)
import           PostgREST.Error                      (pgErrResponse, PgError)
import           PostgREST.Middleware
import           PostgREST.DbStructure

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

  let txSettings = Just (H.ReadCommitted, Just True)
  dbOrError <- H.session pool $ H.tx txSettings $ getDbStructure (cs $ configSchema conf)
  dbStructure <- either hasqlError return dbOrError

  runSettings appSettings $ middle $ \ req respond -> do
    body <- strictRequestBody req
    resOrError <- liftIO $ H.session pool $ H.tx txSettings $
      runWithClaims conf (app dbStructure conf body) req
    either (respond . pgErrResponse) respond resOrError
