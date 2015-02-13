{-# LANGUAGE QuasiQuotes, RankNTypes #-}
module Config (AppConfig, usage, argParser, corsPolicy, ConfigLens) where

import Network.Wai
import Data.Text (strip)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS
import Data.String.Conversions (cs)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..))
import System.Console.GetOpt(OptDescr(..), ArgDescr(..), ArgOrder(RequireOrder),
  getOpt, usageInfo)
import Record(r, l)
import Record.Lens(set, view, Lens)
import Safe(readMay)
import Data.List (intercalate)

type AppConfig = [r| {
    dbName :: String
  , dbPort :: Int
  , dbUser :: String
  , dbPass :: String
  , dbHost :: String

  , port  :: Int
  , anonRole :: String
  , secure :: Bool
  , pool :: Int
  }|]

defaultConf :: AppConfig
defaultConf = [r| {dbName= "", dbPort=5432, dbUser="", dbPass="",
  dbHost="localhost", port=3000, anonRole="", secure=False, pool=10}|]

type ConfigLens a = Lens AppConfig AppConfig a a

options :: [OptDescr (([String], AppConfig) -> ([String], AppConfig))]
options = [
    Option ['a'] ["anonymous"]
      (ReqArg (setval [l|anonRole|]) "ROLE")
      "REQUIRED postgres role for unauthenticated HTTP requests",
    Option ['d'] ["db-name"]
      (ReqArg (setval [l|dbName|]) "NAME")
      "REQUIRED name of database",
    Option ['U'] ["db-user"]
      (ReqArg (setval [l|dbUser|]) "ROLE")
      "REQUIRED postgres role for authenticating requests)",
    Option ['w'] ["db-pass"]
      (ReqArg (setval [l|dbPass|]) "PASS")
      "password for db-user role (default empty password)",
    Option [] ["db-host"]
      (ReqArg (setval [l|dbHost|]) "HOST")
      "postgres server hostname (default localhost)",
    Option ['P'] ["db-port"]
      (ReqArg (setReadVal [l|dbPort|] "db-port must be an integer") "PORT")
      ("postgres server port (default "++(showDefault [l|dbPort|])++")"),
    Option ['p'] ["port"]
      (ReqArg (setReadVal [l|port|] "port must be an integer") "PORT")
      ("postgREST HTTP server port (default "++(showDefault [l|port|])++")"),
    Option ['s'] ["secure"]
      (NoArg (setval [l|secure|] True))
      "Redirect all HTTP requests to HTTPS",
    Option [] ["db-pool"]
      (ReqArg (setReadVal [l|pool|] "pool size must be an integer") "COUNT")
      ("Max connections in database pool (default "++(showDefault [l|pool|])++")"),
    Option ['h'] ["help"] (NoArg id) "show this help"
  ]
    where
      setval :: ConfigLens a -> a -> ([String], AppConfig) -> ([String], AppConfig)
      setval lens val (e, rec) = (e, set lens val rec)
      setReadVal :: (Read a) => ConfigLens a -> String -> String ->
                    ([String], AppConfig) -> ([String], AppConfig)
      setReadVal lens err val (e, rec) = case readMay val of
        Just v -> (e, set lens v rec)
        Nothing -> (err:e, rec)
      showDefault :: (Show a) => ConfigLens a -> String
      showDefault lens = show $ view lens defaultConf

argParser :: [String] -> Either [String] AppConfig
argParser args =
  case getOpt RequireOrder options args of
  (act, _, []) -> let
    (e, c) = (foldr (.) id act) ([], defaultConf)
    mist = missing c
    errs = if null mist then e else (
      "missing required arguments(s): "++(intercalate ", " mist)):e
    in if null errs then Right c else Left errs
  (_, _, errs) -> Left errs

missing :: AppConfig -> [String]
missing conf = foldr (\(get, msg) errs -> if null (get conf) then msg:errs else errs)
  [] [
    (view [l|anonRole|], "[-a|--anonymous]"),
    (view [l|dbName|], "[-d|--db-name]"),
    (view [l|dbUser|], "[-U|--db-user]")]

usage :: String
usage = usageInfo (example ++ "\nArguments:") options
  where example = "Example:\n\
    \  postgrest  --db-host localhost  --db-port 5432     \\\n\
    \             --db-name my_db      --db-user postgres \\\n\
    \             --db-pass foobar     --db-pool 200      \\\n\
    \             --anonymous postgres --secure           \\\n\
    \             --port 3000\n"

defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True)
    , corsRequestHeaders = "Authentication":accHeaders
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . cs . strip . cs) $ BS.split ',' hdrs
      Nothing -> []
