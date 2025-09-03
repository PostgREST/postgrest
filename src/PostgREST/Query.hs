{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : PostgREST.Query
Description : PostgREST query executor

This module parametrizes, prepares, executes SQL queries and decodes their results.

TODO: This module shouldn't depend on SchemaCache: once OpenAPI is removed, this can be done
TOOD: Split the SQL transaction concerns module into another one so Query.hs is pure
-}
module PostgREST.Query
  ( Query (..)
  , QueryResult (..)
  , ResultSet (..)
  , mainTx
  , mainQuery
  , MainQuery (..)
  ) where

import           Control.Lens                      ((^?))
import           Control.Monad.Extra               (whenJust)
import qualified Data.Aeson.Lens                   as L
import qualified Data.ByteString                   as BS hiding
                                                         (break)
import qualified Data.ByteString.Char8             as BS
import qualified Data.HashMap.Strict               as HM
import qualified Data.Set                          as S
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Snippet   as SQL hiding (sql)
import qualified Hasql.DynamicStatements.Statement as SQL
import qualified Hasql.Session                     as SQL (Session)
import qualified Hasql.Transaction                 as SQL
import qualified Hasql.Transaction.Sessions        as SQL

import qualified PostgREST.Error              as Error
import qualified PostgREST.Query.PreQuery     as PreQuery
import qualified PostgREST.Query.QueryBuilder as QueryBuilder
import qualified PostgREST.Query.SqlFragment  as SqlFragment
import qualified PostgREST.Query.Statements   as Statements
import qualified PostgREST.SchemaCache        as SchemaCache


import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Preferences  (PreferCount (..),
                                          PreferHandling (..),
                                          PreferMaxAffected (..),
                                          PreferTransaction (..),
                                          Preferences (..))
import PostgREST.ApiRequest.Types        (Mutation (..))
import PostgREST.Auth.Types              (AuthResult (..))
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Error                   (Error)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (ActionPlan (..),
                                          CallReadPlan (..),
                                          CrudPlan (..),
                                          DbActionPlan (..),
                                          InfoPlan (..),
                                          InspectPlan (..))
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Routine     (Routine (..), RoutineMap)
import PostgREST.SchemaCache.Table       (TablesMap)

import Protolude hiding (Handler)

type DbHandler = ExceptT Error SQL.Transaction

data Query
  = DbQuery {
      dqIsoLevel    :: SQL.IsolationLevel
    , dqTxMode      :: SQL.Mode
    , dqDbHandler   :: DbHandler QueryResult
    , dqTransaction :: SQL.IsolationLevel -> SQL.Mode -> SQL.Transaction (Either Error QueryResult) -> SQL.Session (Either Error QueryResult)
    }
  | NoDbQuery QueryResult

data QueryResult
  = DbCrudResult  CrudPlan ResultSet
  | DbCallResult  CallReadPlan  ResultSet
  | MaybeDbResult InspectPlan  (Maybe (TablesMap, RoutineMap, Maybe Text))
  | NoDbResult    InfoPlan

-- The Queries that run on every request
data MainQuery = MainQuery
  { mqTxVars  :: SQL.Snippet       -- ^ the transaction variables that always run on each query
  , mqPreReq  :: Maybe SQL.Snippet -- ^ the pre-request function that runs if enabled
  , mqCount   :: SQL.Snippet       -- ^ this count query is actually a fragment of the main query, but the same count query also runs after the main one in the case of `count=estimated`, so it's cached here.
  -- TODO only one of the following queries actually runs on each request, once OpenAPI is removed from core it will be easier to refactor this
  , mqMain    :: SQL.Snippet
  , mqOpenAPI :: (SQL.Snippet, SQL.Snippet, SQL.Snippet)
  }

-- | Standard result set format used for all queries
data ResultSet
  = RSStandard
  { rsTableTotal :: Maybe Int64
  -- ^ count of all the table rows
  , rsQueryTotal :: Int64
  -- ^ count of the query rows
  , rsLocation   :: [(BS.ByteString, BS.ByteString)]
  -- ^ The Location header(only used for inserts) is represented as a list of strings containing
  -- variable bindings like @"k1=eq.42"@, or the empty list if there is no location header.
  , rsBody       :: BS.ByteString
  -- ^ the aggregated body of the query
  , rsGucHeaders :: Maybe BS.ByteString
  -- ^ the HTTP headers to be added to the response
  , rsGucStatus  :: Maybe Text
  -- ^ the HTTP status to be added to the response
  , rsInserted   :: Maybe Int64
  -- ^ the number of rows inserted (Only used for upserts)
  }
  | RSPlan BS.ByteString -- ^ the plan of the query

mainTx :: MainQuery -> AppConfig -> AuthResult -> ApiRequest -> ActionPlan -> SchemaCache -> Query
mainTx _ _ _ _ (NoDb x) _ = NoDbQuery $ NoDbResult x
mainTx genQ@MainQuery{..} conf@AppConfig{..} AuthResult{..} apiReq (Db plan) sCache =
  DbQuery isoLvl txMode dbHandler transaction
  where
    transaction = if configDbPreparedStatements then SQL.transaction else SQL.unpreparedTransaction
    isoLvl = planIsoLvl conf authRole plan
    txMode = planTxMode plan
    mainActionQuery = actionQuery genQ plan conf apiReq sCache
    dbHandler = do
      lift $ SQL.statement mempty $ SQL.dynamicallyParameterized mqTxVars
          HD.noResult configDbPreparedStatements
      lift $ whenJust mqPreReq $ \q ->
        SQL.statement mempty $ SQL.dynamicallyParameterized q
          HD.noResult configDbPreparedStatements
      mainActionQuery

planTxMode :: DbActionPlan -> SQL.Mode
planTxMode (DbCrud x)   = pTxMode x
planTxMode (DbCall x)   = crTxMode x
planTxMode (MayUseDb x) = ipTxmode x

planIsoLvl :: AppConfig -> ByteString -> DbActionPlan -> SQL.IsolationLevel
planIsoLvl AppConfig{configRoleIsoLvl} role actPlan = case actPlan of
  DbCall CallReadPlan{crProc} -> fromMaybe roleIsoLvl $ pdIsoLvl crProc
  _                           -> roleIsoLvl
  where
    roleIsoLvl = HM.findWithDefault SQL.ReadCommitted role configRoleIsoLvl

mainQuery :: ActionPlan -> AppConfig -> ApiRequest -> AuthResult -> Maybe QualifiedIdentifier -> MainQuery
mainQuery (NoDb _) _ _ _ _ = MainQuery mempty Nothing mempty mempty (mempty, mempty, mempty)
mainQuery (Db plan) conf@AppConfig{..} apiReq@ApiRequest{iPreferences=Preferences{..}} authRes preReq =
  let genQ = MainQuery (PreQuery.txVarQuery plan conf authRes apiReq) (PreQuery.preReqQuery <$> preReq) in
  case plan of
    DbCrud WrappedReadPlan{..} ->
      let countQuery = QueryBuilder.readPlanToCountQuery wrReadPlan in
      genQ countQuery (Statements.mainRead wrReadPlan countQuery preferCount configDbMaxRows wrMedia wrHandler) (mempty, mempty, mempty)
    DbCrud MutateReadPlan{..} ->
      genQ mempty (Statements.mainWrite mrReadPlan mrMutatePlan mrMedia mrHandler preferRepresentation preferResolution) (mempty, mempty, mempty)
    DbCall CallReadPlan{..} ->
      genQ mempty (Statements.mainCall crProc crCallPlan crReadPlan preferCount crMedia crHandler) (mempty, mempty, mempty)
    MayUseDb InspectPlan{ipSchema=tSchema} ->
      genQ mempty mempty (SqlFragment.accessibleTables tSchema, SqlFragment.accessibleFuncs tSchema, SqlFragment.schemaDescription tSchema)

-- TODO: Generate the Hasql Statement in a diferent module after the OpenAPI functionality is removed
actionQuery :: MainQuery -> DbActionPlan -> AppConfig -> ApiRequest -> SchemaCache -> DbHandler QueryResult
actionQuery MainQuery{..} (DbCrud plan@WrappedReadPlan{..}) conf@AppConfig{..} apiReq _ =
  mainActionQuery
  where
    result = SQL.dynamicallyParameterized mqMain decodeIt configDbPreparedStatements
    mainActionQuery = do
      resultSet <- lift $ SQL.statement mempty result
      failNotSingular wrMedia resultSet
      optionalRollback conf apiReq
      DbCrudResult plan <$> resultSetWTotal conf apiReq resultSet mqCount

    decodeIt :: HD.Result ResultSet
    decodeIt = case wrMedia of
      MTVndPlan{} -> planRow
      _           -> HD.singleRow $ standardRow True

actionQuery MainQuery{..} (DbCrud plan@MutateReadPlan{..}) conf@AppConfig{..} apiReq@ApiRequest{iPreferences=Preferences{..}} _ =
  mainActionQuery
  where
    result = SQL.dynamicallyParameterized mqMain decodeIt configDbPreparedStatements
    failMutation resultSet = case mrMutation of
      MutationCreate -> do
        failNotSingular mrMedia resultSet
      MutationUpdate -> do
        failNotSingular mrMedia resultSet
        failExceedsMaxAffectedPref (preferMaxAffected,preferHandling) resultSet
      MutationSingleUpsert -> do
        failPut resultSet
      MutationDelete -> do
        failNotSingular mrMedia resultSet
        failExceedsMaxAffectedPref (preferMaxAffected,preferHandling) resultSet
    mainActionQuery = do
      resultSet <- lift $ SQL.statement mempty result
      failMutation resultSet
      optionalRollback conf apiReq
      pure $ DbCrudResult plan resultSet

    decodeIt :: HD.Result ResultSet
    decodeIt = case mrMedia of
      MTVndPlan{} -> planRow
      _           -> fromMaybe (RSStandard Nothing 0 mempty mempty Nothing Nothing Nothing) <$> HD.rowMaybe (standardRow False)

actionQuery MainQuery{..} (DbCall plan@CallReadPlan{..}) conf@AppConfig{..} apiReq@ApiRequest{iPreferences=Preferences{..}} _ =
  mainActionQuery
  where
    result = SQL.dynamicallyParameterized mqMain decodeIt configDbPreparedStatements

    mainActionQuery = do
      resultSet <- lift $ SQL.statement mempty result
      optionalRollback conf apiReq
      failNotSingular crMedia resultSet
      failExceedsMaxAffectedPref (preferMaxAffected,preferHandling) resultSet
      pure $ DbCallResult plan resultSet

    decodeIt :: HD.Result ResultSet
    decodeIt = case crMedia of
      MTVndPlan{} -> planRow
      _           -> fromMaybe (RSStandard (Just 0) 0 mempty mempty Nothing Nothing Nothing) <$> HD.rowMaybe (standardRow True)

actionQuery MainQuery{mqOpenAPI=(tblsQ, funcsQ, schQ)} (MayUseDb plan@InspectPlan{ipSchema=tSchema}) AppConfig{..} _ sCache =
  mainActionQuery
  where
    mainActionQuery = lift $
      case configOpenApiMode of
        OAFollowPriv -> do
          tableAccess <- SQL.statement mempty $ SQL.dynamicallyParameterized tblsQ  decodeAccessibleIdentifiers configDbPreparedStatements
          accFuncs <-  SQL.statement mempty $ SQL.dynamicallyParameterized   funcsQ   SchemaCache.decodeFuncs configDbPreparedStatements
          schDesc <- SQL.statement mempty $ SQL.dynamicallyParameterized     schQ decodeSchemaDesc configDbPreparedStatements
          let tbls = HM.filterWithKey (\qi _ -> S.member qi tableAccess) $ SchemaCache.dbTables sCache

          pure $ MaybeDbResult plan (Just (tbls, accFuncs, schDesc))
        OAIgnorePriv -> do
          schDesc <- SQL.statement mempty (SQL.dynamicallyParameterized (SqlFragment.schemaDescription tSchema) decodeSchemaDesc configDbPreparedStatements)

          let tbls = HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) (SchemaCache.dbTables sCache)
              routs = HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) (SchemaCache.dbRoutines sCache)

          pure $ MaybeDbResult plan (Just (tbls, routs, schDesc))
        OADisabled ->
          pure $ MaybeDbResult plan Nothing

    decodeSchemaDesc :: HD.Result (Maybe Text)
    decodeSchemaDesc = join <$> HD.rowMaybe (nullableColumn HD.text)

    decodeAccessibleIdentifiers :: HD.Result (S.Set QualifiedIdentifier)
    decodeAccessibleIdentifiers =
      let
        row = QualifiedIdentifier
          <$> column HD.text
          <*> column HD.text
      in
      S.fromList <$> HD.rowList row

-- Makes sure the querystring pk matches the payload pk
-- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted,
-- PUT /items?id=eq.14 { "id" : 2, .. } is rejected.
-- If this condition is not satisfied then nothing is inserted,
-- check the WHERE for INSERT in QueryBuilder.hs to see how it's done
failPut :: ResultSet -> DbHandler ()
failPut RSPlan{} = pure ()
failPut RSStandard{rsQueryTotal=queryTotal} =
  when (queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.ApiRequestError Error.PutMatchingPkError

resultSetWTotal :: AppConfig -> ApiRequest -> ResultSet -> SQL.Snippet -> DbHandler ResultSet
resultSetWTotal _ _ rs@RSPlan{} _ = return rs
resultSetWTotal AppConfig{..} ApiRequest{iPreferences=Preferences{..}} rs@RSStandard{rsTableTotal=tableTotal} countQuery =
  case preferCount of
    Just PlannedCount -> do
      total <- explain
      return rs{rsTableTotal=total}
    Just EstimatedCount ->
      if tableTotal > (fromIntegral <$> configDbMaxRows) then do
        total <- max tableTotal <$> explain
        return rs{rsTableTotal=total}
      else
        return rs
    Just ExactCount ->
      return rs
    Nothing ->
      return rs
  where
    explain =
      lift . SQL.statement mempty $
        SQL.dynamicallyParameterized (Statements.postExplain countQuery)
        decodeIt
        configDbPreparedStatements

    decodeIt :: HD.Result (Maybe Int64)
    decodeIt =
      let row = HD.singleRow $ column HD.bytea in
      (^? L.nth 0 . L.key "Plan" .  L.key "Plan Rows" . L._Integral) <$> row

-- |
-- Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: MediaType -> ResultSet -> DbHandler ()
failNotSingular _ RSPlan{} = pure ()
failNotSingular mediaType RSStandard{rsQueryTotal=queryTotal} =
  when (elem mediaType [MTVndSingularJSON True, MTVndSingularJSON False] && queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.ApiRequestError . Error.SingularityError $ toInteger queryTotal

failExceedsMaxAffectedPref :: (Maybe PreferMaxAffected, Maybe PreferHandling) -> ResultSet -> DbHandler ()
failExceedsMaxAffectedPref (Nothing,_) _ = pure ()
failExceedsMaxAffectedPref _ RSPlan{} = pure ()
failExceedsMaxAffectedPref (Just (PreferMaxAffected n), handling) RSStandard{rsQueryTotal=queryTotal} = when ((queryTotal > n) && (handling == Just Strict)) $ do
  lift SQL.condemn
  throwError $ Error.ApiRequestError . Error.MaxAffectedViolationError $ toInteger queryTotal

-- | Set a transaction to roll back if requested
optionalRollback :: AppConfig -> ApiRequest -> DbHandler ()
optionalRollback AppConfig{..} ApiRequest{iPreferences=Preferences{..}} = do
  lift $ when (shouldRollback || (configDbTxRollbackAll && not shouldCommit)) $ do
    SQL.sql "SET CONSTRAINTS ALL IMMEDIATE"
    SQL.condemn
  where
    shouldCommit =
      preferTransaction == Just Commit
    shouldRollback =
      preferTransaction == Just Rollback

-- | We use rowList because when doing EXPLAIN (FORMAT TEXT), the result comes as many rows. FORMAT JSON comes as one.
planRow :: HD.Result ResultSet
planRow = RSPlan . BS.unlines <$> HD.rowList (column HD.bytea)

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

arrayColumn :: HD.Value a -> HD.Row [a]
arrayColumn = column . HD.listArray . HD.nonNullable

standardRow :: Bool -> HD.Row ResultSet
standardRow noLocation =
  RSStandard <$> nullableColumn HD.int8 <*> column HD.int8
             <*> (if noLocation then pure mempty else fmap splitKeyValue <$> arrayColumn HD.bytea)
             <*> (fromMaybe mempty <$> nullableColumn HD.bytea)
             <*> nullableColumn HD.bytea
             <*> nullableColumn HD.text
             <*> nullableColumn HD.int8
  where
    splitKeyValue :: ByteString -> (ByteString, ByteString)
    splitKeyValue kv =
      let (k, v) = BS.break (== '=') kv in
      (k, BS.tail v)
