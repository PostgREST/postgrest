{-# LANGUAGE RecordWildCards #-}
{-|
Module      : PostgREST.Query
Description : PostgREST query building

TODO: This module shouldn't depend on SchemaCache: once OpenAPI is removed, this can be done
-}
module PostgREST.Query
  ( mainQuery
  , MainQuery (..)
  ) where

import qualified Hasql.DynamicStatements.Snippet as SQL hiding (sql)

import qualified PostgREST.Query.PreQuery     as PreQuery
import qualified PostgREST.Query.QueryBuilder as QueryBuilder
import qualified PostgREST.Query.SqlFragment  as SqlFragment
import qualified PostgREST.Query.Statements   as Statements


import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Preferences  (Preferences (..),
                                          shouldExplainCount)
import PostgREST.Auth.Types              (AuthResult (..))
import PostgREST.Config                  (AppConfig (..))
import PostgREST.Plan                    (ActionPlan (..),
                                          CrudPlan (..),
                                          DbActionPlan (..),
                                          InspectPlan (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..))

import Protolude hiding (Handler)

-- The Queries that run on every request
data MainQuery = MainQuery
  { mqTxVars  :: SQL.Snippet       -- ^ the transaction variables that always run on each query
  , mqPreReq  :: Maybe SQL.Snippet -- ^ the pre-request function that runs if enabled
  -- TODO only one of the following queries actually runs on each request, once OpenAPI is removed from core it will be easier to refactor this
  , mqMain    :: SQL.Snippet
  , mqOpenAPI :: (SQL.Snippet, SQL.Snippet, SQL.Snippet)
  , mqExplain :: Maybe SQL.Snippet     -- ^ the explain query that gets generated for the "Prefer: count=estimated" case
  }

mainQuery :: ActionPlan -> AppConfig -> ApiRequest -> AuthResult -> Maybe QualifiedIdentifier -> MainQuery
mainQuery (NoDb _) _ _ _ _ = MainQuery mempty Nothing mempty (mempty, mempty, mempty) mempty
mainQuery (Db plan) conf@AppConfig{..} apiReq@ApiRequest{iPreferences=Preferences{..}} authRes preReq =
  let genQ = MainQuery (PreQuery.txVarQuery plan conf authRes apiReq) (PreQuery.preReqQuery <$> preReq) in
  case plan of
    DbCrud _ WrappedReadPlan{..} ->
      let countQuery = QueryBuilder.readPlanToCountQuery wrReadPlan in
      genQ (Statements.mainRead wrReadPlan countQuery preferCount configDbMaxRows pMedia wrHandler) (mempty, mempty, mempty)
      (if shouldExplainCount preferCount then Just (Statements.postExplain countQuery) else Nothing)
    DbCrud _ MutateReadPlan{..} ->
      genQ (Statements.mainWrite mrReadPlan mrMutatePlan pMedia mrHandler preferRepresentation preferResolution) (mempty, mempty, mempty) mempty
    DbCrud _ CallReadPlan{..} ->
      genQ (Statements.mainCall crProc crCallPlan crReadPlan preferCount pMedia crHandler) (mempty, mempty, mempty) mempty
    MayUseDb InspectPlan{ipSchema=tSchema} ->
      genQ mempty (SqlFragment.accessibleTables tSchema, SqlFragment.accessibleFuncs tSchema, SqlFragment.schemaDescription tSchema) mempty
