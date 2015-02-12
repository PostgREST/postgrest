module Config (AppConfig(..), usage, argParser, corsPolicy) where

import Network.Wai
import Data.Text (strip)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BS
import Data.String.Conversions (cs)
-- import Options.Applicative hiding (columns)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..))
import System.Console.GetOpt(OptDescr(..), ArgDescr(..), ArgOrder(RequireOrder),
  getOpt, usageInfo)

data AppConfig = AppConfig {
    configDbName :: String
  , configDbPort :: Int
  , configDbUser :: String
  , configDbPass :: String
  , configDbHost :: String

  , configPort  :: Int
  , configAnonRole :: String
  , configSecure :: Bool
  , configPool :: Int
  } deriving (Eq, Show)

defaultConf :: AppConfig
defaultConf = AppConfig "" 5432 "" "" "localhost" 3000 "" False 10

options :: [OptDescr (AppConfig -> AppConfig)]
options = [
    Option ['a'] ["anonymous"] (ReqArg (\a c -> c {configAnonRole = a}) "ROLE")
      "REQUIRED postgres role for unauthenticated HTTP requests",
    Option ['d'] ["db-name"] (ReqArg (\d c -> c {configDbName = d}) "NAME")
      "REQUIRED name of database",
    Option ['U'] ["db-user"] (ReqArg (\u c -> c {configDbUser = u}) "ROLE")
      "REQUIRED postgres role for authenticating requests)",
    Option ['w'] ["db-pass"] (ReqArg (\p c -> c {configDbPass = p}) "PASS")
      "password for db-user role (default empty password)",
    Option [] ["db-host"] (ReqArg (\h c -> c {configDbHost = h}) "HOST")
      "postgres server hostname (default localhost)",
    Option ['P'] ["db-port"] (ReqArg (\p c -> c {configDbPort = read p}) "PORT")
      ("postgres server port (default"++(show (configDbPort defaultConf))++")"),
    Option ['P'] ["port"] (ReqArg (\p c -> c {configPort = read p}) "PORT")
      ("postgREST HTTP server port (default"++(show (configPort defaultConf))++")"),
    Option ['s'] ["secure"] (NoArg (\c -> c {configSecure = True}))
      "Redirect all HTTP requests to HTTPS",
    Option [] ["db-pool"] (ReqArg (\p c -> c {configPool = read p}) "COUNT")
      "Max connections in database pool",
    Option ['h'] ["help"] (NoArg id) "show this help"
  ]

argParser :: [String] -> Either [String] AppConfig
argParser args =
  case getOpt RequireOrder options args of
  (act, _, []) -> let
      c = (foldr (.) id act) defaultConf
      errs = missing c
    in if null errs then Right c else Left errs
  (_, _, errs) -> Left errs

missing :: AppConfig -> [String]
missing conf = foldr (\(get, msg) errs -> if null (get conf) then msg:errs else errs)
  [] [
    (configAnonRole, "[-a|--anonymous]"),
    (configDbName, "[-d|--db-name]"),
    (configDbUser, "[-U|--db-user]")]

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
