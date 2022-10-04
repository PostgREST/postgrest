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

import qualified PostgREST.DbStructure        as DbStructure
import qualified PostgREST.DbStructure.Proc   as Proc
import qualified PostgREST.Error              as Error
import qualified PostgREST.Query.QueryBuilder as QueryBuilder
import qualified PostgREST.Query.Statements   as Statements
import qualified PostgREST.RangeQuery         as RangeQuery

import Data.Scientific (FPFormat (..), formatScientific, isInteger)

import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Config.PgVersion        (PgVersion (..),
                                          pgVersion140)
import PostgREST.DbStructure             (DbStructure (..))
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)
import PostgREST.DbStructure.Proc        (ProcDescription (..),
                                          ProcVolatility (..),
                                          ProcsMap)
import PostgREST.DbStructure.Table       (TablesMap)
import PostgREST.Error                   (Error)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan.CallPlan           (CallPlan)
import PostgREST.Plan.MutatePlan         (MutatePlan)
import PostgREST.Plan.ReadPlan           (ReadPlanTree)
import PostgREST.Query.SqlFragment       (fromQi, intercalateSnippet,
                                          pgFmtIdentList,
                                          setConfigLocal,
                                          setConfigLocalJson)
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.Request.ApiRequest      (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Target (..))
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferParameters (..),
                                          shouldCount)

import Protolude hiding (Handler)

type DbHandler = ExceptT Error SQL.Transaction

readQuery :: ReadPlanTree -> AppConfig -> ApiRequest -> DbHandler (ResultSet, Maybe Int64)
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
  total <- readTotal conf apiReq resultSet countQuery
  pure (resultSet, total)

readTotal :: AppConfig -> ApiRequest -> ResultSet -> SQL.Snippet -> DbHandler (Maybe Int64)
readTotal _ _ RSPlan{} _ = pure Nothing
readTotal AppConfig{..} ApiRequest{..} RSStandard{rsTableTotal=tableTotal} countQuery =
  case iPreferCount of
    Just PlannedCount ->
      explain
    Just EstimatedCount ->
      if tableTotal > (fromIntegral <$> configDbMaxRows) then
        max tableTotal <$> explain
      else
        return tableTotal
    _ ->
      return tableTotal
  where
    explain =
      lift . SQL.statement mempty . Statements.preparePlanRows countQuery $
        configDbPreparedStatements

createQuery :: MutatePlan -> ReadPlanTree -> [FieldName] -> ApiRequest -> AppConfig -> DbHandler ResultSet
createQuery mutateReq readReq pkCols apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mutateReq readReq True pkCols apiReq conf
  failNotSingular iAcceptMediaType resultSet
  pure resultSet

updateQuery :: MutatePlan -> ReadPlanTree -> ApiRequest -> AppConfig -> DbHandler ResultSet
updateQuery mutateReq readReq apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mutateReq readReq False mempty apiReq conf
  failNotSingular iAcceptMediaType resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet
  pure resultSet

singleUpsertQuery :: MutatePlan -> ReadPlanTree -> ApiRequest -> AppConfig -> DbHandler ResultSet
singleUpsertQuery mutateReq readReq apiReq conf = do
  resultSet <- writeQuery mutateReq readReq False mempty apiReq conf
  failPut resultSet
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

deleteQuery :: MutatePlan -> ReadPlanTree -> ApiRequest -> AppConfig -> DbHandler ResultSet
deleteQuery mutateReq readReq apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mutateReq readReq False mempty apiReq conf
  failNotSingular iAcceptMediaType resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet
  pure resultSet

invokeQuery :: ProcDescription -> CallPlan -> ReadPlanTree -> ApiRequest -> AppConfig -> DbHandler ResultSet
invokeQuery proc callReq readReq ApiRequest{..} AppConfig{..} = do
  resultSet <-
    lift . SQL.statement mempty $
      Statements.prepareCall
        (Proc.procReturnsScalar proc)
        (Proc.procReturnsSingle proc)
        (QueryBuilder.callPlanToQuery callReq)
        (QueryBuilder.readPlanToQuery readReq)
        (QueryBuilder.readPlanToCountQuery readReq)
        (shouldCount iPreferCount)
        iAcceptMediaType
        (iPreferParameters == Just MultipleObjects)
        iBinaryField
        configDbPreparedStatements

  failNotSingular iAcceptMediaType resultSet
  pure resultSet

openApiQuery :: DbStructure -> PgVersion -> AppConfig -> Schema -> DbHandler (Maybe (TablesMap, ProcsMap, Maybe Text))
openApiQuery dbStructure pgVer AppConfig{..} tSchema =
  lift $ case configOpenApiMode of
    OAFollowPriv ->
      Just <$> ((,,)
         <$> SQL.statement [tSchema] (DbStructure.accessibleTables pgVer configDbPreparedStatements)
         <*> SQL.statement tSchema (DbStructure.accessibleProcs pgVer configDbPreparedStatements)
         <*> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements))
    OAIgnorePriv ->
      Just <$> ((,,)
            (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ DbStructure.dbTables dbStructure)
            (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ DbStructure.dbProcs dbStructure)
        <$> SQL.statement tSchema (DbStructure.schemaDescription configDbPreparedStatements))
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

writeQuery :: MutatePlan -> ReadPlanTree -> Bool -> [Text] -> ApiRequest -> AppConfig  -> DbHandler ResultSet
writeQuery mutateReq readReq isInsert pkCols apiReq conf = do
  lift . SQL.statement mempty $
    Statements.prepareWrite
      (QueryBuilder.readPlanToQuery readReq)
      (QueryBuilder.mutatePlanToQuery mutateReq)
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
