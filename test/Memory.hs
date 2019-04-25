module Main where

import           Network.Wai (Application, Request, defaultRequest)
import           SpecHelper  (posgrestTestApp, requestAction, testCfg)

import           Weigh       (Weigh, mainWith, maxAllocs,
                              validateAction)

import           Protolude

main :: IO ()
main = do
  postgrestApp <- posgrestTestApp
  let testMaxAllocationMB = appMaxAlocationTesterMB (postgrestApp testCfg)

  mainWith $
    testMaxAllocationMB "default request" defaultRequest 30


appMaxAlocationTesterMB :: Application -> Text -> Request -> Int64 -> Weigh ()
appMaxAlocationTesterMB app testDescription req maxMB =
  validateAction (toS testDescription) (requestAction app) req (maxAllocs $ maxMB * 1024 * 1024)
