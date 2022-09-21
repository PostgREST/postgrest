{-|
Module      : PostgREST.Middleware
Description : Sets CORS policy. Also the PostgreSQL GUCs, role, search_path and pre-request function.
-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Middleware
  ( optionalRollback
  ) where

import qualified Hasql.Transaction as SQL
import qualified Network.Wai       as Wai


import PostgREST.Config             (AppConfig (..))
import PostgREST.Error              (Error, errorResponseFor)
import PostgREST.GucHeader          (addHeadersIfNotIncluded)
import PostgREST.Request.ApiRequest (ApiRequest (..))

import PostgREST.Request.Preferences

import Protolude

-- | Set a transaction to eventually roll back if requested and set respective
-- headers on the response.
optionalRollback
  :: AppConfig
  -> ApiRequest
  -> ExceptT Error SQL.Transaction Wai.Response
  -> ExceptT Error SQL.Transaction Wai.Response
optionalRollback AppConfig{..} ApiRequest{..} transaction = do
  resp <- catchError transaction $ return . errorResponseFor
  when (shouldRollback || (configDbTxRollbackAll && not shouldCommit)) $ lift do
    SQL.sql "SET CONSTRAINTS ALL IMMEDIATE"
    SQL.condemn
  return $ Wai.mapResponseHeaders preferenceApplied resp
  where
    shouldCommit =
      configDbTxAllowOverride && iPreferTransaction == Just Commit
    shouldRollback =
      configDbTxAllowOverride && iPreferTransaction == Just Rollback
    preferenceApplied
      | shouldCommit =
          addHeadersIfNotIncluded
            [toAppliedHeader Commit]
      | shouldRollback =
          addHeadersIfNotIncluded
            [toAppliedHeader Rollback]
      | otherwise =
          identity
