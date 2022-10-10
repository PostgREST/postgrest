{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Query
  ( createQuery
  , deleteQuery
  , invokeQuery
  , openApiQuery
  , readQuery
  , singleUpsertQuery
  , txMode
  , updateQuery
  , setPgLocals
  , DbHandler
  ) where

import qualified Data.Aeson                        as JSON
import qualified Data.Aeson.Key                    as K
import qualified Data.Aeson.KeyMap                 as KM
import qualified Data.ByteString.Lazy.Char8        as LBS
import qualified Data.HashMap.Strict               as HM
import qualified Data.Text.Encoding                as T
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Snippet   as SQL (Snippet)
import qualified Hasql.DynamicStatements.Statement as SQL
import qualified Hasql.Transaction                 as SQL
import qualified Hasql.Transaction.Sessions        as SQL

import qualified PostgREST.Error              as Error
import qualified PostgREST.Query.QueryBuilder as QueryBuilder
import qualified PostgREST.Query.Statements   as Statements
import qualified PostgREST.RangeQuery         as RangeQuery
import qualified PostgREST.SchemaCache        as SchemaCache
import qualified PostgREST.SchemaCache.Proc   as Proc

import Data.Scientific (FPFormat (..), formatScientific, isInteger)

import PostgREST.ApiRequest              (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Target (..))
import PostgREST.ApiRequest.Preferences  (PreferCount (..),
                                          PreferParameters (..),
                                          PreferTransaction (..),
                                          shouldCount)
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Config.PgVersion        (PgVersion (..),
                                          pgVersion140)
import PostgREST.Error                   (Error)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (CallReadPlan (..),
                                          MutateReadPlan (..))
import PostgREST.Plan.MutatePlan         (MutatePlan (..))
import PostgREST.Plan.ReadPlan           (ReadPlanTree)
import PostgREST.Query.SqlFragment       (fromQi, intercalateSnippet,
                                          pgFmtIdentList,
                                          setConfigLocal,
                                          setConfigLocalJson)
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          Schema)
import PostgREST.SchemaCache.Proc        (ProcDescription (..),
                                          ProcVolatility (..),
                                          ProcsMap)
import PostgREST.SchemaCache.Table       (TablesMap)

import Protolude hiding (Handler)

type DbHandler = ExceptT Error SQL.Transaction

readQuery :: ReadPlanTree -> AppConfig -> ApiRequest -> DbHandler ResultSet
readQuery req conf@AppConfig{..} apiReq@ApiRequest{..} = do
  let countQuery = QueryBuilder.readPlanToCountQuery req
  resultSet <-
     lift . SQL.statement mempty $
      Statements.prepareRead
        (QueryBuilder.readPlanToQuery req)
        (if iPreferCount == Just EstimatedCount then
           -- LIMIT maxRows + 1 so we can determine below that maxRows was surpassed
           QueryBuilder.limitedQuery countQuery ((+ 1) <$> configDbMaxRows)
         else
           countQuery
        )
        (shouldCount iPreferCount)
        iAcceptMediaType
        iBinaryField
        configDbPreparedStatements
  failNotSingular iAcceptMediaType resultSet
  optionalRollback conf apiReq
  resultSetWTotal conf apiReq resultSet countQuery

resultSetWTotal :: AppConfig -> ApiRequest -> ResultSet -> SQL.Snippet -> DbHandler ResultSet
resultSetWTotal _ _ rs@RSPlan{} _ = return rs
resultSetWTotal AppConfig{..} ApiRequest{..} rs@RSStandard{rsTableTotal=tableTotal} countQuery =
  case iPreferCount of
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
      lift . SQL.statement mempty . Statements.preparePlanRows countQuery $
        configDbPreparedStatements

createQuery :: MutateReadPlan -> ApiRequest -> AppConfig -> DbHandler ResultSet
createQuery mrPlan apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failNotSingular iAcceptMediaType resultSet
  optionalRollback conf apiReq
  pure resultSet

updateQuery :: MutateReadPlan -> ApiRequest -> AppConfig -> DbHandler ResultSet
updateQuery mrPlan apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failNotSingular iAcceptMediaType resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet
  optionalRollback conf apiReq
  pure resultSet

singleUpsertQuery :: MutateReadPlan -> ApiRequest -> AppConfig -> DbHandler ResultSet
singleUpsertQuery mrPlan apiReq conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failPut resultSet
  optionalRollback conf apiReq
  pure resultSet

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
    throwError Error.PutMatchingPkError

deleteQuery :: MutateReadPlan -> ApiRequest -> AppConfig -> DbHandler ResultSet
deleteQuery mrPlan apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failNotSingular iAcceptMediaType resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet
  optionalRollback conf apiReq
  pure resultSet

invokeQuery :: ProcDescription -> CallReadPlan -> ApiRequest -> AppConfig -> DbHandler ResultSet
invokeQuery proc CallReadPlan{crReadPlan, crCallPlan} apiReq@ApiRequest{..} conf@AppConfig{..} = do
  resultSet <-
    lift . SQL.statement mempty $
      Statements.prepareCall
        (Proc.procReturnsScalar proc)
        (Proc.procReturnsSingle proc)
        (QueryBuilder.callPlanToQuery crCallPlan)
        (QueryBuilder.readPlanToQuery crReadPlan)
        (QueryBuilder.readPlanToCountQuery crReadPlan)
        (shouldCount iPreferCount)
        iAcceptMediaType
        (iPreferParameters == Just MultipleObjects)
        iBinaryField
        configDbPreparedStatements

  optionalRollback conf apiReq
  failNotSingular iAcceptMediaType resultSet
  pure resultSet

openApiQuery :: SchemaCache -> PgVersion -> AppConfig -> Schema -> DbHandler (Maybe (TablesMap, ProcsMap, Maybe Text))
openApiQuery sCache pgVer AppConfig{..} tSchema =
  lift $ case configOpenApiMode of
    OAFollowPriv ->
      Just <$> ((,,)
         <$> SQL.statement [tSchema] (SchemaCache.accessibleTables pgVer configDbPreparedStatements)
         <*> SQL.statement tSchema (SchemaCache.accessibleProcs pgVer configDbPreparedStatements)
         <*> SQL.statement tSchema (SchemaCache.schemaDescription configDbPreparedStatements))
    OAIgnorePriv ->
      Just <$> ((,,)
            (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ SchemaCache.dbTables sCache)
            (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ SchemaCache.dbProcs sCache)
        <$> SQL.statement tSchema (SchemaCache.schemaDescription configDbPreparedStatements))
    OADisabled ->
      pure Nothing

txMode :: ApiRequest -> SQL.Mode
txMode ApiRequest{..} =
  case (iAction, iTarget) of
    (ActionRead _, _) ->
      SQL.Read
    (ActionInspect _, _) ->
      SQL.Read
    (ActionInvoke InvGet, _) ->
      SQL.Read
    (ActionInvoke InvHead, _) ->
      SQL.Read
    (ActionInvoke InvPost, TargetProc ProcDescription{pdVolatility=Stable} _) ->
      SQL.Read
    (ActionInvoke InvPost, TargetProc ProcDescription{pdVolatility=Immutable} _) ->
      SQL.Read
    _ ->
      SQL.Write

writeQuery :: MutateReadPlan -> ApiRequest -> AppConfig  -> DbHandler ResultSet
writeQuery MutateReadPlan{mrReadPlan, mrMutatePlan} apiReq conf =
  let
    (isInsert, pkCols) = case mrMutatePlan of {Insert{insPkCols} -> (True, insPkCols); _ -> (False, mempty);}
  in
  lift . SQL.statement mempty $
    Statements.prepareWrite
      (QueryBuilder.readPlanToQuery mrReadPlan)
      (QueryBuilder.mutatePlanToQuery mrMutatePlan)
      isInsert
      (iAcceptMediaType apiReq)
      (iPreferRepresentation apiReq)
      pkCols
      (configDbPreparedStatements conf)

-- |
-- Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: MediaType -> ResultSet -> DbHandler ()
failNotSingular _ RSPlan{} = pure ()
failNotSingular mediaType RSStandard{rsQueryTotal=queryTotal} =
  when (mediaType == MTSingularJSON && queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.singularityError queryTotal

failsChangesOffLimits :: Maybe Integer -> ResultSet -> DbHandler ()
failsChangesOffLimits _ RSPlan{} = pure ()
failsChangesOffLimits Nothing _  = pure ()
failsChangesOffLimits (Just maxChanges) RSStandard{rsQueryTotal=queryTotal} =
  when (queryTotal > fromIntegral maxChanges) $ do
    lift SQL.condemn
    throwError $ Error.OffLimitsChangesError queryTotal maxChanges

-- | Set a transaction to roll back if requested
optionalRollback :: AppConfig -> ApiRequest -> DbHandler ()
optionalRollback AppConfig{..} ApiRequest{..} = do
  lift $ when (shouldRollback || (configDbTxRollbackAll && not shouldCommit)) $ do
    SQL.sql "SET CONSTRAINTS ALL IMMEDIATE"
    SQL.condemn
  where
    shouldCommit =
      configDbTxAllowOverride && iPreferTransaction == Just Commit
    shouldRollback =
      configDbTxAllowOverride && iPreferTransaction == Just Rollback

-- | Runs local(transaction scoped) GUCs for every request, plus the pre-request function
setPgLocals :: AppConfig   -> KM.KeyMap JSON.Value -> Text ->
               ApiRequest  -> ByteString -> PgVersion -> DbHandler ()
setPgLocals conf claims role req jsonDbS actualPgVersion = do
  lift $ SQL.statement mempty $ SQL.dynamicallyParameterized
    ("select " <> intercalateSnippet ", " (searchPathSql : roleSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ appSettingsSql ++ specSql))
    HD.noResult (configDbPreparedStatements conf)
  lift $ traverse_ SQL.sql preReqSql
  where
    methodSql = setConfigLocal mempty ("request.method", iMethod req)
    pathSql = setConfigLocal mempty ("request.path", iPath req)
    headersSql = if usesLegacyGucs
                   then setConfigLocal "request.header." <$> iHeaders req
                   else setConfigLocalJson "request.headers" (iHeaders req)
    cookiesSql = if usesLegacyGucs
                   then setConfigLocal "request.cookie." <$> iCookies req
                   else setConfigLocalJson "request.cookies" (iCookies req)
    claimsSql = if usesLegacyGucs
                  then setConfigLocal "request.jwt.claim." <$> [(toUtf8 $ K.toText c, toUtf8 $ unquoted v) | (c,v) <- KM.toList claims]
                  else [setConfigLocal mempty ("request.jwt.claims", LBS.toStrict $ JSON.encode claims)]
    roleSql = [setConfigLocal mempty ("role", toUtf8 role)]
    appSettingsSql = setConfigLocal mempty <$> (join bimap toUtf8 <$> configAppSettings conf)
    searchPathSql =
      let schemas = pgFmtIdentList (iSchema req : configDbExtraSearchPath conf) in
      setConfigLocal mempty ("search_path", schemas)
    preReqSql = (\f -> "select " <> fromQi f <> "();") <$> configDbPreRequest conf
    specSql = case iTarget req of
      TargetProc{tpIsRootSpec=True} -> [setConfigLocal mempty ("request.spec", jsonDbS)]
      _                             -> mempty
    usesLegacyGucs = configDbUseLegacyGucs conf && actualPgVersion < pgVersion140

    unquoted :: JSON.Value -> Text
    unquoted (JSON.String t) = t
    unquoted (JSON.Number n) =
      toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
    unquoted (JSON.Bool b) = show b
    unquoted v = T.decodeUtf8 . LBS.toStrict $ JSON.encode v
