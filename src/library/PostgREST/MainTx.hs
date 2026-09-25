{-# LANGUAGE FlexibleInstances #-}
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
import Hasql.Encoders qualified as HE
import Hasql.Pipeline qualified as P
import Hasql.Session qualified as SQL (Session, pipeline, sql, statement)
import Hasql.Statement (Statement (..))
import Hasql.Transaction.Sessions qualified as SQL (IsolationLevel (..), Mode (..))
import PostgREST.Error qualified as Error
import PostgREST.SchemaCache qualified as SchemaCache

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
  { rsTableTotal :: Maybe Int64
  -- ^ count of all the table rows
  , rsQueryTotal :: Int64
  -- ^ count of the query rows
  , rsLocation :: [(BS.ByteString, BS.ByteString)]
  -- ^ The Location header(only used for inserts) is represented as a list of strings containing
  -- variable bindings like @"k1=eq.42"@, or the empty list if there is no location header.
  , rsBody :: BS.ByteString
  -- ^ the aggregated body of the query
  , rsGucHeaders :: Maybe BS.ByteString
  -- ^ the HTTP headers to be added to the response
  , rsGucStatus :: Maybe Text
  -- ^ the HTTP status to be added to the response
  , rsInserted :: Maybe Int64
  -- ^ the number of rows inserted (Only used for upserts)
  }

-- | The result set a query that matched no row yields.
emptyResultSet :: Maybe Int64 -> ResultSet
emptyResultSet tableTotal = RSStandard tableTotal 0 mempty mempty Nothing Nothing Nothing

-- | Statement transport. A 'SQL.Session' sends one statement and waits for its
-- result before sending the next. 'P.Pipeline' sends them all and reads all the
-- results back with one round trip. No result can be seen mid-batch.
class Applicative t => Issues t where
  issue :: Statement () a -> t a

instance Issues SQL.Session where
  issue = SQL.statement ()

instance Issues P.Pipeline where
  issue = P.statement ()

-- | A transaction-control statement. libpq rejects the simple query protocol
-- inside a pipeline, so these cannot go through 'SQL.sql' -- they have to be
-- extended-protocol statements like every other one. They are marked
-- non-preparable: a utility statement has no plan worth reusing, and it keeps the
-- batch off the prepared-statement path whatever db-prepared-statements says.
control :: ByteString -> Statement () ()
control statementSql = Statement statementSql HE.noParams HD.noResult False

-- | Mirrors the BEGIN that hasql transaction issues verbatim.
beginStmt :: SQL.IsolationLevel -> SQL.Mode -> Statement () ()
beginStmt isoLvl mode = control $ "BEGIN " <> isolation <> " " <> access
  where
    isolation = case isoLvl of
      SQL.ReadCommitted -> "ISOLATION LEVEL READ COMMITTED"
      SQL.RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
      SQL.Serializable -> "ISOLATION LEVEL SERIALIZABLE"
    access = case mode of
      SQL.Write -> "READ WRITE"
      SQL.Read -> "READ ONLY"

-- | How the transaction ends. 'TxAbort' carries whether deferred constraints are
-- checked first, which is what still surfaces a constraint violation on a request
-- that asked not to commit.
data TxEnd
  = TxCommit
  | TxAbort Bool

terminate :: Issues t => TxEnd -> t ()
terminate TxCommit = issue $ control "COMMIT"
terminate (TxAbort checkDeferred) =
  when checkDeferred (issue $ control "SET CONSTRAINTS ALL IMMEDIATE") *> issue (control "ABORT")

-- | The point at which a requested rollback takes effect, decides whether deferred
-- constraints are checked before the abort.
data ResultStep
  = FailWhen (ResultSet -> Maybe Error)
  | MaybeRollback

-- | Interprets a plan's steps: the request's error if any, and how the transaction ends
-- A failed check aborts, which is what 'SQL.condemn' used to do at each of these places
applySteps :: Bool -> [ResultStep] -> ResultSet -> (Maybe Error, TxEnd)
applySteps wantRollback steps resultSet = go TxCommit steps
  where
    go end [] = (Nothing, end)
    go end (MaybeRollback : rest)
      | wantRollback = go (TxAbort True) rest
      | otherwise = go end rest
    go end (FailWhen failsWhen : rest) = case failsWhen resultSet of
      Just err -> (Just err, abort end)
      Nothing -> go end rest
    abort (TxAbort checkDeferred) = TxAbort checkDeferred
    abort TxCommit = TxAbort False

-- | The steps a plan applies after its main statement, in order. A check is only
-- included when the request can actually trip it, so a request with default
-- preferences carries none -- which is what lets its terminator ride along in the
-- pipeline.
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
      | elem mediaType [MTVndSingularJSON True, MTVndSingularJSON False] = [FailWhen notSingular]
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

mainTx :: MainQuery -> AppConfig -> AuthResult -> ApiRequest -> ActionPlan -> SchemaCache -> MainTx
mainTx _ _ _ _ (NoDb x) _ = NoDbTx $ NoDbResult x
mainTx genQ conf@AppConfig{configDbPipelineMode} AuthResult{authRole} apiReq (Db plan) sCache =
  DbTx . rollbackOnError $ if configDbPipelineMode then pipelined else sequential
  where
    txMode = planTxMode plan
    begin = beginStmt (planIsoLvl conf authRole plan) txMode
    steps = resultSteps plan apiReq
    wantRollback = rollbackRequested conf apiReq

    body :: Issues t => t (Either Error DbResult, TxEnd)
    body = dbBody genQ plan conf apiReq sCache steps wantRollback

    -- The terminator can share the batch whenever the choice between COMMIT and
    -- ABORT cannot depend on a result, which is exactly when no check inspects the
    -- result set. That is what puts an ordinary request on a single round trip; a
    -- PUT, singular JSON, and a strict max-affected preference pay a second one.
    -- READ ONLY does not widen this. A read-only transaction still admits effects
    -- whose visibility depends on the terminator -- NOTIFY is permitted in one and
    -- delivered only on COMMIT, and so is a write to a temporary table already on
    -- the connection -- so a failed check must abort there too, exactly as
    -- upstream's SQL.condemn did.
    terminatorIsStatic = not (any inspectsResult steps)
    inspectsResult (FailWhen _) = True
    inspectsResult MaybeRollback = False

    pipelined
      | terminatorIsStatic =
          SQL.pipeline $
            issue begin *> (fst <$> body) <* terminate staticEnd
      | otherwise = do
          (res, end) <- SQL.pipeline $ issue begin *> body
          terminate end
          pure res
      where
        -- Read from applySteps like the dynamic path does, so the two cannot disagree:
        -- a plan with no MaybeRollback step commits even when a rollback was requested.
        -- Being static means no FailWhen present, so the result set here is never inspected.
        staticEnd = snd $ applySteps wantRollback steps (emptyResultSet Nothing)

    sequential = do
      issue begin
      (res, end) <- body
      terminate end
      pure res

-- | Cleans up after a failed statement. An error inside a pipeline for BEGIN..COMMIT
-- leaves the session in an aborted transaction. Postgres treats ROLLBACK with no transaction
-- in progress as a no-op, so this is equally safe on a path that never opened one.
rollbackOnError :: SQL.Session a -> SQL.Session a
rollbackOnError session = session `catchError` \err -> SQL.sql "ROLLBACK" >> throwError err

planTxMode :: DbActionPlan -> SQL.Mode
planTxMode (DbCrud _ x) = pTxMode x
planTxMode (MayUseDb x) = ipTxmode x

planIsoLvl :: AppConfig -> ByteString -> DbActionPlan -> SQL.IsolationLevel
planIsoLvl AppConfig{configRoleIsoLvl} role actPlan = case actPlan of
  DbCrud _ CallReadPlan{crProc} -> fromMaybe roleIsoLvl $ pdIsoLvl crProc
  _ -> roleIsoLvl
  where
    roleIsoLvl = HM.findWithDefault SQL.ReadCommitted role configRoleIsoLvl

-- | Every statement the request needs, as one Applicative chain: the transaction
-- variables, the optional pre-request, then the plan's own queries. It yields the
-- request's outcome together with how the transaction must end, both derived from
-- the result set without a further round trip.
dbBody
  :: Issues t
  => MainQuery
  -> DbActionPlan
  -> AppConfig
  -> ApiRequest
  -> SchemaCache
  -> [ResultStep]
  -> Bool
  -> t (Either Error DbResult, TxEnd)
dbBody MainQuery{..} actPlan AppConfig{..} ApiRequest{iPreferences = Preferences{..}} sCache steps wantRollback =
  issue (dynStmt mqTxVars HD.noResult)
    *> traverse_ (\q -> issue $ dynStmt q HD.noResult) mqPreReq
    *> planBody
  where
    dynStmt snippet decoder =
      SQL.dynamicallyParameterized snippet decoder configDbPreparedStatements

    outcome resultSet res = case applySteps wantRollback steps resultSet of
      (Just err, end) -> (Left err, end)
      (Nothing, end) -> (Right res, end)

    planBody = case actPlan of
      -- EXPLAIN yields the plan text rather than a result set, and has no check to
      -- run against one.
      DbCrud True plan ->
        (\explRes -> outcome (emptyResultSet Nothing) $ DbPlanResult (pMedia plan) explRes)
          <$> issue (dynStmt mqMain planRow)
      DbCrud _ plan@WrappedReadPlan{} ->
        (\resultSet explainTotal -> outcome resultSet . DbCrudResult plan $ counted explainTotal resultSet)
          <$> issue (dynStmt mqMain (HD.singleRow $ standardRow True))
          <*> (join <$> traverse (\snippet -> issue $ dynStmt snippet decodeExplain) mqExplain)
      DbCrud _ plan@MutateReadPlan{} ->
        (\resultSet -> outcome resultSet $ DbCrudResult plan resultSet)
          <$> issue (dynStmt mqMain $ rowOr (emptyResultSet Nothing) False)
      DbCrud _ plan@CallReadPlan{} ->
        (\resultSet -> outcome resultSet $ DbCrudResult plan resultSet)
          <$> issue (dynStmt mqMain $ rowOr (emptyResultSet (Just 0)) True)
      MayUseDb plan -> openApiBody plan

    rowOr dflt noLocation = fromMaybe dflt <$> HD.rowMaybe (standardRow noLocation)

    counted explainTotal resultSet@RSStandard{rsTableTotal = tableTotal} =
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

    decodeExplain :: HD.Result (Maybe Int64)
    decodeExplain =
      let row = HD.singleRow $ column HD.bytea
      in  (^? L.nth 0 . L.key "Plan" . L.key "Plan Rows" . L._Integral) <$> row

    -- The three privilege queries are independent of each other, so they batch
    -- together into the same round trip as the rest of the request.
    openApiBody plan@InspectPlan{ipSchema = tSchema} = case configOpenApiMode of
      OAFollowPriv ->
        ( \tableAccess accFuncs schDesc ->
            let tbls = HM.filterWithKey (\qi _ -> S.member qi tableAccess) $ SchemaCache.dbTables sCache
            in  inspected plan $ Just (tbls, accFuncs, schDesc)
        )
          <$> issue (dynStmt tblsQ decodeAccessibleIdentifiers)
          <*> issue (dynStmt funcsQ SchemaCache.decodeFuncs)
          <*> issue (dynStmt schQ decodeSchemaDesc)
      OAIgnorePriv ->
        (\schDesc -> inspected plan $ Just (inSchema $ SchemaCache.dbTables sCache, inSchema $ SchemaCache.dbRoutines sCache, schDesc))
          <$> issue (dynStmt schQ decodeSchemaDesc)
      OADisabled ->
        pure $ inspected plan Nothing
      where
        (tblsQ, funcsQ, schQ) = mqOpenAPI
        inspected p = outcome (emptyResultSet Nothing) . MaybeDbResult p
        inSchema :: HM.HashMap QualifiedIdentifier v -> HM.HashMap QualifiedIdentifier v
        inSchema = HM.filterWithKey (\(QualifiedIdentifier sch _) _ -> sch == tSchema)

decodeSchemaDesc :: HD.Result (Maybe Text)
decodeSchemaDesc = join <$> HD.rowMaybe (nullableColumn HD.text)

decodeAccessibleIdentifiers :: HD.Result (S.Set QualifiedIdentifier)
decodeAccessibleIdentifiers =
  let row =
        QualifiedIdentifier
          <$> column HD.text
          <*> column HD.text
  in  S.fromList <$> HD.rowList row

-- | We use rowList because when doing EXPLAIN (FORMAT TEXT), the result comes as many rows. FORMAT JSON comes as one.
planRow :: HD.Result BS.ByteString
planRow = BS.unlines <$> HD.rowList (column HD.bytea)

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

arrayColumn :: HD.Value a -> HD.Row [a]
arrayColumn = column . HD.listArray . HD.nonNullable

standardRow :: Bool -> HD.Row ResultSet
standardRow noLocation =
  RSStandard <$> nullableColumn HD.int8
    <*> column HD.int8
    <*> (if noLocation then pure mempty else fmap splitKeyValue <$> arrayColumn HD.bytea)
    <*> (fromMaybe mempty <$> nullableColumn HD.bytea)
    <*> nullableColumn HD.bytea
    <*> nullableColumn HD.text
    <*> nullableColumn HD.int8
  where
    splitKeyValue :: ByteString -> (ByteString, ByteString)
    splitKeyValue kv =
      let (k, v) = BS.break (== '=') kv
      in  (k, BS.tail v)
