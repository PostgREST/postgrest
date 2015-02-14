module Main where

import Paths_postgrest (version)

import App
import Middleware
import Error(errResponse)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Network.Wai (strictRequestBody)
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.Static (staticPolicy, only)
import Data.List (intercalate)
import Data.Version (versionBranch)
import qualified Hasql as H
import qualified Hasql.Postgres as P
import Data.Monoid
import System.Exit(exitFailure, exitSuccess)

import Config (AppConfig, usage, corsPolicy, argParser)
import System.Environment(getArgs)
import System.Console.GetOpt(OptDescr(..), ArgDescr(..), ArgOrder(Permute),
  getOpt)
import Record(l)
import Record.Lens(view)

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute [Option "?h" ["help"] (NoArg ()) "show this help"] args of
    ([()], _, _) -> help []
    _ -> case argParser args of
           Left errs -> help errs
           Right conf -> runApp conf

help :: [String] -> IO ()
help [] = putStr usage >> exitSuccess
help errs = do
  putStr $ intercalate "\n" errs ++ '\n':'\n':usage
  exitFailure

runApp::AppConfig->IO()
runApp conf = do
  let port = view [l|port|] conf
  let secure = view [l|secure|] conf

  unless secure $
    putStrLn "WARNING, running in insecure mode, auth will be in plaintext"
  Prelude.putStrLn $ "Listening on port " ++ show port

  let pgSettings = P.ParamSettings (cs $ view [l|dbHost|] conf)
                     (fromIntegral $ view [l|dbPort|] conf)
                     (cs $ view [l|dbUser|] conf)
                     (cs $ view [l|dbPass|] conf)
                     (cs $ view [l|dbName|] conf)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings
      middle =
        (if secure then redirectInsecure else id)
        . gzip def . cors corsPolicy
        . staticPolicy (only [("favicon.ico", "static/favicon.ico")])
      anonRole = cs $ view [l|anonRole|] conf
      currRole = cs $ view [l|dbUser|] conf

  poolSettings <- maybe (fail "Improper session settings") return $
                H.poolSettings (fromIntegral $ view [l|pool|] conf) 30
  pool :: H.Pool P.Postgres <- H.acquirePool pgSettings poolSettings

  runSettings appSettings $ middle $ \req respond -> do
    body <- strictRequestBody req
    resOrError <- liftIO $ H.session pool $ H.tx Nothing $
      authenticated currRole anonRole (app body) req
    either (respond . errResponse) respond resOrError

  where
    prettyVersion = intercalate "." $ map show $ versionBranch version
