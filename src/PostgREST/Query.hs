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
import qualified Data.Aeson.KeyMap                 as KM
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy.Char8        as LBS
import qualified Data.HashMap.Strict               as HM
import qualified Data.Set                          as S
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

import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Preferences  (PreferCount (..),
                                          PreferHandling (..),
                                          PreferMaxAffected (..),
                                          PreferTimezone (..),
                                          PreferTransaction (..),
                                          Preferences (..),
                                          shouldCount)
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Config.PgVersion        (PgVersion (..))
import PostgREST.Error                   (Error)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (CallReadPlan (..),
                                          MutateReadPlan (..),
                                          WrappedReadPlan (..))
import PostgREST.Plan.MutatePlan         (MutatePlan (..))
import PostgREST.Query.SqlFragment       (escapeIdentList, fromQi,
                                          intercalateSnippet,
                                          setConfigWithConstantName,
                                          setConfigWithConstantNameJSON,
                                          setConfigWithDynamicName)
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
        wrHandler
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
updateQuery mrPlan@MutateReadPlan{mrMedia} apiReq@ApiRequest{iPreferences=Preferences{..}, ..} conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failNotSingular mrMedia resultSet
  failExceedsMaxAffectedPref (preferMaxAffected,preferHandling) resultSet
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
deleteQuery mrPlan@MutateReadPlan{mrMedia} apiReq@ApiRequest{iPreferences=Preferences{..}, ..} conf = do
  resultSet <- writeQuery mrPlan apiReq conf
  failNotSingular mrMedia resultSet
  failExceedsMaxAffectedPref (preferMaxAffected,preferHandling) resultSet
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
        crHandler
        configDbPreparedStatements

  optionalRollback conf apiReq
  failNotSingular crMedia resultSet
  failExceedsMaxAffectedPref (preferMaxAffected,preferHandling) resultSet
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
writeQuery MutateReadPlan{..} ApiRequest{iPreferences=Preferences{..}} conf =
  let
    (isPut, isInsert, pkCols) = case mrMutatePlan of {Insert{where_,insPkCols} -> ((not . null) where_, True, insPkCols); _ -> (False,False, mempty);}
  in
  lift . SQL.statement mempty $
    Statements.prepareWrite
      (QueryBuilder.readPlanToQuery mrReadPlan)
      (QueryBuilder.mutatePlanToQuery mrMutatePlan)
      isInsert
      isPut
      mrMedia
      mrHandler
      preferRepresentation
      preferResolution
      pkCols
      (configDbPreparedStatements conf)

-- |
-- Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: MediaType -> ResultSet -> DbHandler ()
failNotSingular _ RSPlan{} = pure ()
failNotSingular mediaType RSStandard{rsQueryTotal=queryTotal} =
  when (elem mediaType [MTVndSingularJSON True, MTVndSingularJSON False] && queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.ApiRequestError . ApiRequestTypes.SingularityError $ toInteger queryTotal

failExceedsMaxAffectedPref :: (Maybe PreferMaxAffected, Maybe PreferHandling) -> ResultSet -> DbHandler ()
failExceedsMaxAffectedPref (Nothing,_) _ = pure ()
failExceedsMaxAffectedPref _ RSPlan{} = pure ()
failExceedsMaxAffectedPref (Just (PreferMaxAffected n), handling) RSStandard{rsQueryTotal=queryTotal} = when ((queryTotal > n) && (handling == Just Strict)) $ do
  lift SQL.condemn
  throwError $ Error.ApiRequestError . ApiRequestTypes.MaxAffectedViolationError $ toInteger queryTotal

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

-- | Set transaction scoped settings
setPgLocals :: AppConfig  -> KM.KeyMap JSON.Value -> BS.ByteString -> [(ByteString, ByteString)] ->
               [(Text,Text)] -> ApiRequest -> DbHandler ()
setPgLocals AppConfig{..} claims role roleSettings funcSettings ApiRequest{..} = lift $
  SQL.statement mempty $ SQL.dynamicallyParameterized
    -- To ensure `GRANT SET ON PARAMETER <superuser_setting> TO authenticator` works, the role settings must be set before the impersonated role.
    -- Otherwise the GRANT SET would have to be applied to the impersonated role. See https://github.com/PostgREST/postgrest/issues/3045
    ("select " <> intercalateSnippet ", " (searchPathSql : roleSettingsSql ++ roleSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ timezoneSql ++ funcSettingsSql ++ appSettingsSql))
    HD.noResult configDbPreparedStatements
  where
    methodSql = setConfigWithConstantName ("request.method", iMethod)
    pathSql = setConfigWithConstantName ("request.path", iPath)
    headersSql = setConfigWithConstantNameJSON "request.headers" iHeaders
    cookiesSql = setConfigWithConstantNameJSON "request.cookies" iCookies
    claimsSql = [setConfigWithConstantName ("request.jwt.claims", LBS.toStrict $ JSON.encode claims)]
    roleSql = [setConfigWithConstantName ("role", role)]
    roleSettingsSql = setConfigWithDynamicName <$> roleSettings
    appSettingsSql = setConfigWithDynamicName <$> (join bimap toUtf8 <$> configAppSettings)
    timezoneSql = maybe mempty (\(PreferTimezone tz) -> [setConfigWithConstantName ("timezone", tz)]) $ preferTimezone iPreferences
    funcSettingsSql = setConfigWithDynamicName <$> (join bimap toUtf8 <$> funcSettings)
    searchPathSql =
      let schemas = escapeIdentList (iSchema : configDbExtraSearchPath) in
      setConfigWithConstantName ("search_path", schemas)

-- | Runs the pre-request function.
runPreReq :: AppConfig -> DbHandler ()
runPreReq conf = lift $ traverse_ (SQL.statement mempty . stmt) (configDbPreRequest conf)
  where
    stmt req = SQL.dynamicallyParameterized
      ("select " <> fromQi req <> "()")
      HD.noResult
      (configDbPreparedStatements conf)
