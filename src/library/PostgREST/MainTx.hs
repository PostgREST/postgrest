{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PostgREST.MainTx
-- Description : PostgREST transaction executor
--
-- This module parametrizes, prepares, executes SQL queries and decodes their results.
module PostgREST.MainTx
  ( MainTx (..)
  , DbResult (..)
  , ResultSet (..)
  , mainTx
  )
where

import Control.Lens ((^?))
import Control.Monad.Extra (whenJust)
import Protolude hiding (Handler)

import Data.Aeson.Lens qualified as L
import Data.ByteString qualified as BS hiding (break)
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S

import PostgREST.ApiRequest (ApiRequest (..))
import PostgREST.ApiRequest.Preferences
  ( PreferCount (..)
  , PreferHandling (..)
  , PreferMaxAffected (..)
  , PreferTransaction (..)
  , Preferences (..)
  )
import PostgREST.ApiRequest.Types (Mutation (..))
import PostgREST.Auth.Types (AuthResult (..))
import PostgREST.Catalog.Identifiers (QualifiedIdentifier (..))
import PostgREST.Catalog.Routine (Routine (..), RoutineMap)
import PostgREST.Catalog.Table (TablesMap)
import PostgREST.Config (AppConfig (..), OpenAPIMode (..))
import PostgREST.Error (Error)
import PostgREST.MediaType (MediaType (..))
import PostgREST.Plan
  ( ActionPlan (..)
  , CrudPlan (..)
  , DbActionPlan (..)
  , InfoPlan (..)
  , InspectPlan (..)
  )
import PostgREST.Query (MainQuery (..))
import PostgREST.SchemaCache (SchemaCache (..))

import Hasql.Decoders qualified as HD
import Hasql.DynamicStatements.Statement qualified as SQL
import Hasql.Helpers qualified as HH
import Hasql.Session qualified as SQL (Session)
import Hasql.Transaction qualified as SQL
import Hasql.Transaction.Sessions qualified as SQL
import PostgREST.Error qualified as Error
import PostgREST.SchemaCache qualified as SchemaCache

type DbHandler = ExceptT Error SQL.Transaction

data MainTx
  = DbTx (SQL.Session (Either Error DbResult))
  | NoDbTx DbResult

data DbResult
  = DbCrudResult CrudPlan ResultSet
  | DbPlanResult MediaType BS.ByteString
  | MaybeDbResult InspectPlan (Maybe (TablesMap, RoutineMap, Maybe Text))
  | NoDbResult InfoPlan

-- | Standard result set format used for the mqMain query
data ResultSet
  = RSStandard
  { -- | count of all the table rows
    rsTableTotal :: Maybe Int64,
    -- | count of the query rows
    rsQueryTotal :: Int64,
    -- | The Location header(only used for inserts) is represented as a list of strings containing
    -- variable bindings like @"k1=eq.42"@, or the empty list if there is no location header.
    rsLocation :: [(BS.ByteString, BS.ByteString)],
    -- | the aggregated body of the query
    rsBody :: BS.ByteString,
    -- | the HTTP headers to be added to the response
    rsGucHeaders :: Maybe BS.ByteString,
    -- | the HTTP status to be added to the response
    rsGucStatus :: Maybe Text,
    -- | the number of rows inserted (Only used for upserts)
    rsInserted :: Maybe Int64
  }

-- | The result set a query that matched no row yields.
emptyResultSet :: Maybe Int64 -> ResultSet
emptyResultSet tableTotal = RSStandard tableTotal 0 mempty mempty Nothing Nothing Nothing

-- | A step applied after the plan's main statement. 'FailWhen' is a predicate on
-- the result set that, when it returns 'Just', fails the request and rolls the
-- transaction back. 'MaybeRollback' honors a 'Prefer: tx=rollback' request (or
-- @db-tx-end=rollback-allow-override@) by checking deferred constraints and
-- aborting instead of committing.
data ResultStep
  = FailWhen (ResultSet -> Maybe Error)
  | MaybeRollback

-- | The steps a plan applies after its main statement, in order.
resultSteps :: DbActionPlan -> ApiRequest -> [ResultStep]
resultSteps (MayUseDb _) _ = []
resultSteps (DbCrud True _) _ = [MaybeRollback]
resultSteps (DbCrud _ plan) ApiRequest{iPreferences = Preferences{..}} = case plan of
  WrappedReadPlan{pMedia} -> singular pMedia ++ [MaybeRollback]
  MutateReadPlan{pMedia, mrMutation} -> case mrMutation of
    MutationCreate -> singular pMedia ++ [MaybeRollback]
    MutationUpdate -> singular pMedia ++ maxAffected ++ [MaybeRollback]
    MutationSingleUpsert -> [FailWhen matchingPk, MaybeRollback]
    MutationDelete -> singular pMedia ++ maxAffected ++ [MaybeRollback]
  CallReadPlan{pMedia} -> MaybeRollback : singular pMedia ++ maxAffected
  where
    -- Fail a response if a single JSON object was requested and not exactly one
    -- was found.
    singular mediaType
      | mediaType `elem` [MTVndSingularJSON True, MTVndSingularJSON False] = [FailWhen notSingular]
      | otherwise = []
    notSingular RSStandard{rsQueryTotal = queryTotal}
      | queryTotal /= 1 = Just . Error.ApiRequestErr . Error.SingularityError $ toInteger queryTotal
      | otherwise = Nothing

    maxAffected = case (preferMaxAffected, preferHandling) of
      (Just (PreferMaxAffected n), Just Strict) -> [FailWhen $ exceedsMaxAffected n]
      _ -> []
    exceedsMaxAffected n RSStandard{rsQueryTotal = queryTotal}
      | queryTotal > n = Just . Error.ApiRequestErr . Error.MaxAffectedViolationError $ toInteger queryTotal
      | otherwise = Nothing

    -- Makes sure the querystring pk matches the payload pk
    -- e.g. PUT /items?id=eq.1 { "id" : 1, .. } is accepted,
    -- PUT /items?id=eq.14 { "id" : 2, .. } is rejected.
    -- If this condition is not satisfied then nothing is inserted,
    -- check the WHERE for INSERT in QueryBuilder.hs to see how it's done
    matchingPk RSStandard{rsQueryTotal = queryTotal}
      | queryTotal /= 1 = Just $ Error.ApiRequestErr Error.PutMatchingPkError
      | otherwise = Nothing

-- | Whether the request asked for the transaction to be rolled back.
rollbackRequested :: AppConfig -> ApiRequest -> Bool
rollbackRequested AppConfig{configDbTxRollbackAll} ApiRequest{iPreferences = Preferences{..}} =
  shouldRollback || (configDbTxRollbackAll && not shouldCommit)
  where
    shouldCommit = preferTransaction == Just Commit
    shouldRollback = preferTransaction == Just Rollback

-- | Interpret the plan's steps against the result set. On a failed check, condemn
-- the transaction and surface the error. On 'MaybeRollback' with a rollback
-- requested, set deferred constraints immediate before condemning so a
-- constraint violation still surfaces on a request that asked not to commit.
applyStepsM :: Bool -> [ResultStep] -> ResultSet -> DbHandler ()
applyStepsM wantRollback steps resultSet = traverse_ runStep steps
  where
    runStep MaybeRollback =
      when wantRollback . lift $ do
        SQL.sql "SET CONSTRAINTS ALL IMMEDIATE"
        SQL.condemn
    runStep (FailWhen failsWhen) = whenJust (failsWhen resultSet) $ \err -> do
      lift SQL.condemn
      throwError err

mainTx :: MainQuery -> AppConfig -> AuthResult -> ApiRequest -> ActionPlan -> SchemaCache -> MainTx
mainTx _ _ _ _ (NoDb x) _ = NoDbTx $ NoDbResult x
mainTx genQ@MainQuery{..} conf@AppConfig{..} AuthResult{..} apiReq (Db plan) sCache =
  DbTx $ SQL.transactionNoRetry isoLvl txMode $ runExceptT dbHandler
  where
    isoLvl = planIsoLvl conf authRole plan
    txMode = planTxMode plan
    dbHandler = do
      lift $
        SQL.statement mempty $
          SQL.dynamicallyParameterized
            mqTxVars
            HD.noResult
            configDbPreparedStatements
      lift $ whenJust mqPreReq $ \q ->
        SQL.statement mempty $
          SQL.dynamicallyParameterized
            q
            HD.noResult
            configDbPreparedStatements
      actionResult genQ plan conf apiReq sCache

planTxMode :: DbActionPlan -> SQL.Mode
planTxMode (DbCrud _ x) = pTxMode x
planTxMode (MayUseDb x) = ipTxmode x

planIsoLvl :: AppConfig -> ByteString -> DbActionPlan -> SQL.IsolationLevel
planIsoLvl AppConfig{configRoleIsoLvl} role actPlan = case actPlan of
  DbCrud _ CallReadPlan{crProc} -> fromMaybe roleIsoLvl $ pdIsoLvl crProc
  _ -> roleIsoLvl
  where
    roleIsoLvl = HM.findWithDefault SQL.ReadCommitted role configRoleIsoLvl

actionResult :: MainQuery -> DbActionPlan -> AppConfig -> ApiRequest -> SchemaCache -> ExceptT Error SQL.Transaction DbResult
actionResult MainQuery{..} actPlan@(DbCrud True plan) conf@AppConfig{..} apiReq _ = do
  explRes <- lift $ SQL.statement mempty $ SQL.dynamicallyParameterized mqMain planRow configDbPreparedStatements
  applyStepsM (rollbackRequested conf apiReq) (resultSteps actPlan apiReq) (emptyResultSet Nothing)
  pure $ DbPlanResult (pMedia plan) explRes
actionResult MainQuery{..} actPlan@(DbCrud _ plan@WrappedReadPlan{}) conf@AppConfig{..} apiReq@ApiRequest{iPreferences = Preferences{..}} _ = do
  resultSet@RSStandard{rsTableTotal = tableTotal} <- lift $ SQL.statement mempty $ dynStmt (HD.singleRow $ standardRow True)
  applyStepsM (rollbackRequested conf apiReq) (resultSteps actPlan apiReq) resultSet
  explainTotal <-
    lift . fmap join $
      traverse
        ( \snip ->
            SQL.statement mempty $ SQL.dynamicallyParameterized snip decodeExplain configDbPreparedStatements
        )
        mqExplain

  pure $
    DbCrudResult
      plan
      resultSet
        { rsTableTotal = case preferCount of
            Just PlannedCount -> explainTotal
            Just EstimatedCount ->
              if tableTotal > (fromIntegral <$> configDbMaxRows) then
                max <$> tableTotal <*> explainTotal
              else
                tableTotal
            _ -> tableTotal
        }
  where
    dynStmt decod = SQL.dynamicallyParameterized mqMain decod configDbPreparedStatements

    decodeExplain :: HD.Result (Maybe Int64)
    decodeExplain =
      let row = HD.singleRow $ HH.column HD.bytea
      in  (^? L.nth 0 . L.key "Plan" . L.key "Plan Rows" . L._Integral) <$> row
actionResult MainQuery{..} actPlan@(DbCrud _ plan@MutateReadPlan{}) conf@AppConfig{..} apiReq _ = do
  resultSet <- lift $ SQL.statement mempty $ dynStmt decodeRow
  applyStepsM (rollbackRequested conf apiReq) (resultSteps actPlan apiReq) resultSet
  pure $ DbCrudResult plan resultSet
  where
    dynStmt decod = SQL.dynamicallyParameterized mqMain decod configDbPreparedStatements
    decodeRow = fromMaybe (emptyResultSet Nothing) <$> HD.rowMaybe (standardRow False)
actionResult MainQuery{..} actPlan@(DbCrud _ plan@CallReadPlan{}) conf@AppConfig{..} apiReq _ = do
  resultSet <- lift $ SQL.statement mempty $ dynStmt decodeRow
  applyStepsM (rollbackRequested conf apiReq) (resultSteps actPlan apiReq) resultSet
  pure $ DbCrudResult plan resultSet
  where
    dynStmt decod = SQL.dynamicallyParameterized mqMain decod configDbPreparedStatements
    decodeRow = fromMaybe (emptyResultSet (Just 0)) <$> HD.rowMaybe (standardRow True)
actionResult MainQuery{mqOpenAPI = (tblsQ, funcsQ, schQ)} (MayUseDb plan@InspectPlan{ipSchema = tSchema}) AppConfig{..} _ sCache =
  mainActionQuery
  where
    mainActionQuery = lift $
      case configOpenApiMode of
        OAFollowPriv -> do
          tableAccess <- SQL.statement mempty $ SQL.dynamicallyParameterized tblsQ decodeAccessibleIdentifiers configDbPreparedStatements
          accFuncs <- SQL.statement mempty $ SQL.dynamicallyParameterized funcsQ SchemaCache.decodeFuncs configDbPreparedStatements
          schDesc <- SQL.statement mempty $ SQL.dynamicallyParameterized schQ decodeSchemaDesc configDbPreparedStatements
          let tbls = HM.filterWithKey (\qi _ -> S.member qi tableAccess) $ SchemaCache.dbTables sCache

          pure $ MaybeDbResult plan (Just (tbls, accFuncs, schDesc))
        OAIgnorePriv -> do
          schDesc <- SQL.statement mempty (SQL.dynamicallyParameterized schQ decodeSchemaDesc configDbPreparedStatements)

          let
            tbls = HM.filterWithKey (\(QualifiedIdentifier sch _) _ -> sch == tSchema) (SchemaCache.dbTables sCache)
            routs = HM.filterWithKey (\(QualifiedIdentifier sch _) _ -> sch == tSchema) (SchemaCache.dbRoutines sCache)

          pure $ MaybeDbResult plan (Just (tbls, routs, schDesc))
        OADisabled ->
          pure $ MaybeDbResult plan Nothing

    decodeSchemaDesc :: HD.Result (Maybe Text)
    decodeSchemaDesc = join <$> HD.rowMaybe (HH.nullableColumn HD.text)

    decodeAccessibleIdentifiers :: HD.Result (S.Set QualifiedIdentifier)
    decodeAccessibleIdentifiers =
      let row =
            QualifiedIdentifier
              <$> HH.column HD.text
              <*> HH.column HD.text
      in  S.fromList <$> HD.rowList row

-- | We use rowList because when doing EXPLAIN (FORMAT TEXT), the result comes as many rows. FORMAT JSON comes as one.
planRow :: HD.Result BS.ByteString
planRow = BS.unlines <$> HD.rowList (HH.column HD.bytea)

standardRow :: Bool -> HD.Row ResultSet
standardRow noLocation =
  RSStandard <$> HH.nullableColumn HD.int8
    <*> HH.column HD.int8
    <*> (if noLocation then pure mempty else fmap splitKeyValue <$> HH.arrayColumn HD.bytea)
    <*> (fromMaybe mempty <$> HH.nullableColumn HD.bytea)
    <*> HH.nullableColumn HD.bytea
    <*> HH.nullableColumn HD.text
    <*> HH.nullableColumn HD.int8
  where
    splitKeyValue :: ByteString -> (ByteString, ByteString)
    splitKeyValue kv =
      let (k, v) = BS.break (== '=') kv
      in  (k, BS.tail v)
