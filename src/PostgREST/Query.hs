{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Query
  ( createQuery
  , deleteQuery
  , invokeQuery
  , openApiQuery
  , readQuery
  , singleUpsertQuery
  , updateQuery
  , setPgLocals
  , runPreReq
  , DbHandler
  ) where

import qualified Data.Aeson                        as JSON
import qualified Data.Aeson.Key                    as K
import qualified Data.Aeson.KeyMap                 as KM
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy.Char8        as LBS
import qualified Data.HashMap.Strict               as HM
import qualified Data.Set                          as S
import qualified Data.Text.Encoding                as T
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Snippet   as SQL (Snippet)
import qualified Hasql.DynamicStatements.Statement as SQL
import qualified Hasql.Transaction                 as SQL

import qualified PostgREST.ApiRequest.Types   as ApiRequestTypes
import qualified PostgREST.Error              as Error
import qualified PostgREST.Query.QueryBuilder as QueryBuilder
import qualified PostgREST.Query.Statements   as Statements
import qualified PostgREST.RangeQuery         as RangeQuery
import qualified PostgREST.SchemaCache        as SchemaCache

import Data.Scientific (FPFormat (..), formatScientific, isInteger)

import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Preferences  (PreferCount (..),
                                          PreferTransaction (..),
                                          Preferences (..),
                                          shouldCount)
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Config.PgVersion        (PgVersion (..),
                                          pgVersion140)
import PostgREST.Error                   (Error)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (CallReadPlan (..),
                                          MutateReadPlan (..),
                                          WrappedReadPlan (..))
import PostgREST.Plan.MutatePlan         (MutatePlan (..))
import PostgREST.Query.SqlFragment       (escapeIdentList, fromQi,
                                          intercalateSnippet,
                                          setConfigLocal,
                                          setConfigLocalJson)
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          Schema)
import PostgREST.SchemaCache.Routine     (Routine (..), RoutineMap)
import PostgREST.SchemaCache.Table       (TablesMap)

import Protolude hiding (Handler)

type DbHandler = ExceptT Error SQL.Transaction

readQuery :: WrappedReadPlan -> AppConfig -> ApiRequest -> DbHandler ResultSet
readQuery WrappedReadPlan{..} conf@AppConfig{..} apiReq@ApiRequest{iPreferences=Preferences{..}} = do
  let countQuery = QueryBuilder.readPlanToCountQuery wrReadPlan
  resultSet <-
     lift . SQL.statement mempty $
      Statements.prepareRead
        (QueryBuilder.readPlanToQuery wrReadPlan)
        (if preferCount == Just EstimatedCount then
           -- LIMIT maxRows + 1 so we can determine below that maxRows was surpassed
           QueryBuilder.limitedQuery countQuery ((+ 1) <$> configDbMaxRows)
         else
           countQuery
        )
        (shouldCount preferCount)
        wrMedia
        wrResAgg
        configDbPreparedStatements
  failNotSingular wrMedia resultSet
  optionalRollback conf apiReq
  resultSetWTotal conf apiReq resultSet countQuery

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
      lift . SQL.statement mempty . Statements.preparePlanRows countQuery $
        configDbPreparedStatements

createQuery :: MutateReadPlan -> ApiRequest -> AppConfig -> DbHandler ResultSet
createQuery mrPlan@MutateReadPlan{mrMedia} apiReq conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failNotSingular mrMedia resultSet
  optionalRollback conf apiReq
  pure resultSet

updateQuery :: MutateReadPlan -> ApiRequest -> AppConfig -> DbHandler ResultSet
updateQuery mrPlan@MutateReadPlan{mrMedia} apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failNotSingular mrMedia resultSet
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
    throwError $ Error.ApiRequestError ApiRequestTypes.PutMatchingPkError

deleteQuery :: MutateReadPlan -> ApiRequest -> AppConfig -> DbHandler ResultSet
deleteQuery mrPlan@MutateReadPlan{mrMedia} apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failNotSingular mrMedia resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet
  optionalRollback conf apiReq
  pure resultSet

invokeQuery :: Routine -> CallReadPlan -> ApiRequest -> AppConfig -> PgVersion -> DbHandler ResultSet
invokeQuery rout CallReadPlan{..} apiReq@ApiRequest{iPreferences=Preferences{..}} conf@AppConfig{..} pgVer = do
  resultSet <-
    lift . SQL.statement mempty $
      Statements.prepareCall
        rout
        (QueryBuilder.callPlanToQuery crCallPlan pgVer)
        (QueryBuilder.readPlanToQuery crReadPlan)
        (QueryBuilder.readPlanToCountQuery crReadPlan)
        (shouldCount preferCount)
        crMedia
        crResAgg
        configDbPreparedStatements

  optionalRollback conf apiReq
  failNotSingular crMedia resultSet
  pure resultSet

openApiQuery :: SchemaCache -> PgVersion -> AppConfig -> Schema -> DbHandler (Maybe (TablesMap, RoutineMap, Maybe Text))
openApiQuery sCache pgVer AppConfig{..} tSchema =
  lift $ case configOpenApiMode of
    OAFollowPriv -> do
      tableAccess <- SQL.statement [tSchema] (SchemaCache.accessibleTables pgVer configDbPreparedStatements)
      Just <$> ((,,)
            (HM.filterWithKey (\qi _ -> S.member qi tableAccess) $ SchemaCache.dbTables sCache)
        <$> SQL.statement tSchema (SchemaCache.accessibleFuncs pgVer configDbPreparedStatements)
        <*> SQL.statement tSchema (SchemaCache.schemaDescription configDbPreparedStatements))
    OAIgnorePriv ->
      Just <$> ((,,)
            (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ SchemaCache.dbTables sCache)
            (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ SchemaCache.dbRoutines sCache)
        <$> SQL.statement tSchema (SchemaCache.schemaDescription configDbPreparedStatements))
    OADisabled ->
      pure Nothing

writeQuery :: MutateReadPlan -> ApiRequest -> AppConfig  -> DbHandler ResultSet
writeQuery MutateReadPlan{mrReadPlan, mrMutatePlan, mrResAgg, mrMedia} ApiRequest{iPreferences=Preferences{..}} conf =
  let
    (isInsert, pkCols) = case mrMutatePlan of {Insert{insPkCols} -> (True, insPkCols); _ -> (False, mempty);}
  in
  lift . SQL.statement mempty $
    Statements.prepareWrite
      (QueryBuilder.readPlanToQuery mrReadPlan)
      (QueryBuilder.mutatePlanToQuery mrMutatePlan)
      isInsert
      mrMedia
      mrResAgg
      preferRepresentation
      pkCols
      (configDbPreparedStatements conf)

-- |
-- Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: MediaType -> ResultSet -> DbHandler ()
failNotSingular _ RSPlan{} = pure ()
failNotSingular mediaType RSStandard{rsQueryTotal=queryTotal} =
  when (elem mediaType [MTSingularJSON True,MTSingularJSON False] && queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.ApiRequestError . ApiRequestTypes.SingularityError $ toInteger queryTotal

failsChangesOffLimits :: Maybe Integer -> ResultSet -> DbHandler ()
failsChangesOffLimits _ RSPlan{} = pure ()
failsChangesOffLimits Nothing _  = pure ()
failsChangesOffLimits (Just maxChanges) RSStandard{rsQueryTotal=queryTotal} =
  when (queryTotal > fromIntegral maxChanges) $ do
    lift SQL.condemn
    throwError $ Error.ApiRequestError $ ApiRequestTypes.OffLimitsChangesError queryTotal maxChanges

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

-- | Runs local (transaction scoped) GUCs for every request.
setPgLocals :: AppConfig  -> KM.KeyMap JSON.Value -> BS.ByteString -> [(ByteString, ByteString)] ->
               ApiRequest -> PgVersion -> DbHandler ()
setPgLocals AppConfig{..} claims role roleSettings req actualPgVersion = lift $
  SQL.statement mempty $ SQL.dynamicallyParameterized
    ("select " <> intercalateSnippet ", " (searchPathSql : roleSql ++ roleSettingsSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ appSettingsSql))
    HD.noResult configDbPreparedStatements
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
    roleSql = [setConfigLocal mempty ("role", role)]
    roleSettingsSql = setConfigLocal mempty <$> roleSettings
    appSettingsSql = setConfigLocal mempty <$> (join bimap toUtf8 <$> configAppSettings)
    searchPathSql =
      let schemas = escapeIdentList (iSchema req : configDbExtraSearchPath) in
      setConfigLocal mempty ("search_path", schemas)
    usesLegacyGucs = configDbUseLegacyGucs && actualPgVersion < pgVersion140

    unquoted :: JSON.Value -> Text
    unquoted (JSON.String t) = t
    unquoted (JSON.Number n) =
      toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
    unquoted (JSON.Bool b) = show b
    unquoted v = T.decodeUtf8 . LBS.toStrict $ JSON.encode v

-- | Runs the pre-request function.
runPreReq :: AppConfig -> DbHandler ()
runPreReq conf = lift $ traverse_ (SQL.statement mempty . stmt) (configDbPreRequest conf)
  where
    stmt req = SQL.dynamicallyParameterized
      ("select " <> fromQi req <> "()")
      HD.noResult
      (configDbPreparedStatements conf)
