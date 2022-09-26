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
  , DbHandler
  ) where

import qualified Data.HashMap.Strict             as HM
import qualified Hasql.DynamicStatements.Snippet as SQL (Snippet)
import qualified Hasql.Transaction               as SQL
import qualified Hasql.Transaction.Sessions      as SQL

import qualified PostgREST.DbStructure         as DbStructure
import qualified PostgREST.DbStructure.Proc    as Proc
import qualified PostgREST.Error               as Error
import qualified PostgREST.Query.QueryBuilder  as QueryBuilder
import qualified PostgREST.Query.Statements    as Statements
import qualified PostgREST.RangeQuery          as RangeQuery
import qualified PostgREST.Request.MutateQuery as MutateRequest
import qualified PostgREST.Request.Types       as ApiRequestTypes

import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Config.PgVersion        (PgVersion (..))
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
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.Request.ApiRequest      (Action (..),
                                          ApiRequest (..),
                                          InvokeMethod (..),
                                          Target (..))
import PostgREST.Request.Preferences     (PreferCount (..),
                                          PreferParameters (..),
                                          shouldCount)
import PostgREST.Request.ReadQuery       (ReadRequest)

import Protolude hiding (Handler)

type DbHandler = ExceptT Error SQL.Transaction

readQuery :: ReadRequest -> AppConfig -> ApiRequest -> Maybe FieldName -> DbHandler (ResultSet, Maybe Int64)
readQuery req conf@AppConfig{..} apiReq@ApiRequest{..} bField = do
  let countQuery = QueryBuilder.readRequestToCountQuery req
  resultSet <-
     lift . SQL.statement mempty $
      Statements.prepareRead
        (QueryBuilder.readRequestToQuery req)
        (if iPreferCount == Just EstimatedCount then
           -- LIMIT maxRows + 1 so we can determine below that maxRows was surpassed
           QueryBuilder.limitedQuery countQuery ((+ 1) <$> configDbMaxRows)
         else
           countQuery
        )
        (shouldCount iPreferCount)
        iAcceptMediaType
        bField
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

createQuery :: MutateRequest.MutateRequest -> ReadRequest -> [FieldName] -> ApiRequest -> AppConfig -> DbHandler ResultSet
createQuery mutateReq readReq pkCols apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mutateReq readReq True pkCols apiReq conf
  failNotSingular iAcceptMediaType resultSet
  pure resultSet

updateQuery :: MutateRequest.MutateRequest -> ReadRequest -> ApiRequest -> AppConfig -> DbHandler ResultSet
updateQuery mutateReq readReq apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mutateReq readReq False mempty apiReq conf
  failNotSingular iAcceptMediaType resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet
  pure resultSet

singleUpsertQuery :: MutateRequest.MutateRequest -> ReadRequest -> ApiRequest -> AppConfig -> DbHandler ResultSet
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

deleteQuery :: MutateRequest.MutateRequest -> ReadRequest -> ApiRequest -> AppConfig -> DbHandler ResultSet
deleteQuery mutateReq readReq apiReq@ApiRequest{..} conf = do
  resultSet <- writeQuery mutateReq readReq False mempty apiReq conf
  failNotSingular iAcceptMediaType resultSet
  failsChangesOffLimits (RangeQuery.rangeLimit iTopLevelRange) resultSet
  pure resultSet

invokeQuery :: ProcDescription -> ApiRequestTypes.CallRequest -> ReadRequest -> ApiRequest -> Maybe FieldName -> AppConfig -> DbHandler ResultSet
invokeQuery proc callReq readReq ApiRequest{..} bField AppConfig{..} = do
  resultSet <-
    lift . SQL.statement mempty $
      Statements.prepareCall
        (Proc.procReturnsScalar proc)
        (Proc.procReturnsSingle proc)
        (QueryBuilder.requestToCallProcQuery callReq)
        (QueryBuilder.readRequestToQuery readReq)
        (QueryBuilder.readRequestToCountQuery readReq)
        (shouldCount iPreferCount)
        iAcceptMediaType
        (iPreferParameters == Just MultipleObjects)
        bField
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
    (ActionInfo, _) ->
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

writeQuery :: MutateRequest.MutateRequest -> ReadRequest -> Bool -> [Text] -> ApiRequest -> AppConfig  -> DbHandler ResultSet
writeQuery mutateReq readReq isInsert pkCols apiReq conf = do
  lift . SQL.statement mempty $
    Statements.prepareWrite
      (QueryBuilder.readRequestToQuery readReq)
      (QueryBuilder.mutateRequestToQuery mutateReq)
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

