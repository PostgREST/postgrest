module Main where

import qualified Hasql.Pool                 as P
import qualified Hasql.Transaction.Sessions as HT

import           Control.AutoUpdate         (defaultUpdateSettings,
                                             mkAutoUpdate,
                                             updateAction)
import           Data.IORef                 (newIORef)
import           Data.Time.Clock            (getCurrentTime)
import           Network.HTTP.Types.Status  (statusCode)
import           Network.Wai                (Application, Request,
                                             defaultRequest)
import           Network.Wai.Test           (request, runSession,
                                             simpleStatus)
import           PostgREST.App              (postgrest)
import           PostgREST.Config           (AppConfig)
import           PostgREST.DbStructure      (getDbStructure,
                                             getPgVersion)
import           SpecHelper                 (getEnvVarWithDefault,
                                             setupDb, testCfg)

import           Weigh                      (Weigh, mainWith,
                                             maxAllocs,
                                             validateAction)

import           Protolude

main :: IO ()
main = do
  postgrestApp <- posgrestTestApp testCfg
  let testMaxAllocationMB = appMaxAlocationTesterMB postgrestApp

  mainWith $
    testMaxAllocationMB "default request" defaultRequest 30


appMaxAlocationTesterMB :: Application -> Text -> Request -> Int64 -> Weigh ()
appMaxAlocationTesterMB app testDescription req maxMB =
  validateAction (toS testDescription) (requestAction app) req $ maxAllocs $ maxMB * 1024 * 1024

requestAction :: Application -> Request -> IO Int
requestAction app req = do
  response <- runSession (request req) app
  return . statusCode . simpleStatus $ response

posgrestTestApp :: (Text -> AppConfig) -> IO Application
posgrestTestApp config = do
  testDbConn <- getEnvVarWithDefault "POSTGREST_TEST_CONNECTION" "postgres://postgrest_test@localhost/postgrest_test"
  setupDb testDbConn

  pool <- P.acquire (3, 10, toS testDbConn)

  result <- P.use pool $ do
    ver <- getPgVersion
    HT.transaction HT.ReadCommitted HT.Read $ getDbStructure "test" ver

  dbStructure <- pure $ either (panic.show) identity result

  getTime <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime }

  refDbStructure <- newIORef $ Just dbStructure

  return $ postgrest (config testDbConn) refDbStructure pool getTime $ pure ()
