{-|
Module      : PostgREST.Request.DbRequestBuilder
Description : PostgREST database request builder

This module is in charge of building an intermediate
representation(ReadRequest, MutateRequest) between the HTTP request and the
final resulting SQL query.

A query tree is built in case of resource embedding. By inferring the
relationship between tables, join conditions are added for every embedded
resource.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module PostgREST.Request.DbRequestBuilder
  ( readRequest
  , mutateRequest
  , callRequest
  ) where

import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

import Data.Either.Combinators (mapLeft)
import Data.List               (delete)
import Data.Tree               (Tree (..))

import PostgREST.DbStructure.Identifiers  (FieldName,
                                           QualifiedIdentifier (..),
                                           Schema, TableName)
import PostgREST.DbStructure.Proc         (ProcDescription (..),
                                           ProcParam (..),
                                           procReturnsScalar)
import PostgREST.DbStructure.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..))
import PostgREST.DbStructure.Table        (Column (..), Table (..),
                                           tableQi)
import PostgREST.Error                    (Error (..))
import PostgREST.Query.SqlFragment        (sourceCTEName)
import PostgREST.RangeQuery               (NonnegRange, allRange,
                                           restrictRange)
import PostgREST.Request.ApiRequest       (Action (..),
                                           ApiRequest (..),
                                           InvokeMethod (..),
                                           Mutation (..),
                                           Payload (..))

import PostgREST.Request.Preferences
import PostgREST.Request.Types

import qualified PostgREST.DbStructure.Relationship as Relationship
import qualified PostgREST.Request.QueryParams      as QueryParams

import Protolude hiding (from)

-- | Builds the ReadRequest tree on a number of stages.
-- | Adds filters, order, limits on its respective nodes.
-- | Adds joins conditions obtained from resource embedding.
readRequest :: Schema -> TableName -> Maybe Integer -> [Relationship] -> ApiRequest -> Either Error ReadRequest
readRequest schema rootTableName maxRows allRels apiRequest  =
  mapLeft ApiRequestError $
  treeRestrictRange maxRows (iAction apiRequest) =<<
  augmentRequestWithJoin schema rootRels =<<
  addLogicTrees apiRequest =<<
  addRanges apiRequest =<<
  addOrders apiRequest =<<
  addFilters apiRequest (initReadRequest rootName qsSelect)
  where
    QueryParams.QueryParams{..} = iQueryParams apiRequest
    (rootName, rootRels) = rootWithRels schema rootTableName allRels (iAction apiRequest)

-- Get the root table name with its relationships according to the Action type.
-- This is done because of the shape of the final SQL Query. The mutation cases
-- are wrapped in a WITH {sourceCTEName}(see Statements.hs).  So we need a FROM
-- {sourceCTEName} instead of FROM {tableName}.
rootWithRels :: Schema -> TableName -> [Relationship] -> Action -> (QualifiedIdentifier, [Relationship])
rootWithRels schema rootTableName allRels action = case action of
  ActionRead _ -> (QualifiedIdentifier schema rootTableName, allRels) -- normal read case
  _            -> (QualifiedIdentifier mempty _sourceCTEName, mapMaybe toSourceRel allRels ++ allRels) -- mutation cases and calling proc
  where
    _sourceCTEName = decodeUtf8 sourceCTEName
    -- To enable embedding in the sourceCTEName cases we need to replace the
    -- foreign key tableName in the Relationship with {sourceCTEName}. This way
    -- findRel can find relationships with sourceCTEName.
    toSourceRel :: Relationship -> Maybe Relationship
    toSourceRel r@Relationship{relTable=t}
      | rootTableName == tableName t = Just $ r {relTable=t {tableName=_sourceCTEName}}
      | otherwise                    = Nothing

-- Build the initial tree with a Depth attribute so when a self join occurs we
-- can differentiate the parent and child tables by having an alias like
-- "table_depth", this is related to
-- http://github.com/PostgREST/postgrest/issues/987.
initReadRequest :: QualifiedIdentifier -> [Tree SelectItem] -> ReadRequest
initReadRequest rootQi =
  foldr (treeEntry rootDepth) initial
  where
    rootDepth = 0
    rootSchema = qiSchema rootQi
    rootName = qiName rootQi
    initial = Node (Select [] rootQi Nothing [] [] [] [] allRange, (rootName, Nothing, Nothing, Nothing, Nothing, rootDepth)) []
    treeEntry :: Depth -> Tree SelectItem -> ReadRequest -> ReadRequest
    treeEntry depth (Node fld@((fn, _),_,alias, hint, joinType) fldForest) (Node (q, i) rForest) =
      let nxtDepth = succ depth in
      case fldForest of
        [] -> Node (q {select=fld:select q}, i) rForest
        _  -> Node (q, i) $
              foldr (treeEntry nxtDepth)
              (Node (Select [] (QualifiedIdentifier rootSchema fn) Nothing [] [] [] [] allRange,
                (fn, Nothing, alias, hint, joinType, nxtDepth)) [])
              fldForest:rForest

-- | Enforces the `max-rows` config on the result
treeRestrictRange :: Maybe Integer -> Action -> ReadRequest -> Either ApiRequestError ReadRequest
treeRestrictRange _ (ActionMutate _) request = Right request
treeRestrictRange maxRows _ request = pure $ nodeRestrictRange maxRows <$> request
  where
    nodeRestrictRange :: Maybe Integer -> ReadNode -> ReadNode
    nodeRestrictRange m (q@Select {range_=r}, i) = (q{range_=restrictRange m r }, i)

augmentRequestWithJoin :: Schema -> [Relationship] -> ReadRequest -> Either ApiRequestError ReadRequest
augmentRequestWithJoin schema allRels request =
  addRels schema allRels Nothing request
  >>= addJoinConditions Nothing

addRels :: Schema -> [Relationship] -> Maybe ReadRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addRels schema allRels parentNode (Node (query@Select{from=tbl}, (nodeName, _, alias, hint, joinType, depth)) forest) =
  case parentNode of
    Just (Node (Select{from=parentNodeQi}, _) _) ->
      let newFrom r = if qiName tbl == nodeName then tableQi (relForeignTable r) else tbl
          newReadNode = (\r -> (query{from=newFrom r}, (nodeName, Just r, alias, hint, joinType, depth))) <$> rel
          rel = findRel schema allRels (qiName parentNodeQi) nodeName hint
      in
      Node <$> newReadNode <*> (updateForest . hush $ Node <$> newReadNode <*> pure forest)
    _ ->
      let rn = (query, (nodeName, Nothing, alias, Nothing, joinType, depth)) in
      Node rn <$> updateForest (Just $ Node rn forest)
  where
    updateForest :: Maybe ReadRequest -> Either ApiRequestError [ReadRequest]
    updateForest rq = addRels schema allRels rq `traverse` forest

-- Finds a relationship between an origin and a target in the request:
-- /origin?select=target(*) If more than one relationship is found then the
-- request is ambiguous and we return an error.  In that case the request can
-- be disambiguated by adding precision to the target or by using a hint:
-- /origin?select=target!hint(*) The elements will be matched according to
-- these rules:
-- origin = table / view
-- target = table / view / constraint / column-from-origin
-- hint   = table / view / constraint / column-from-origin / column-from-target
-- (hint can take table / view values to aid in finding the junction in an m2m relationship)
findRel :: Schema -> [Relationship] -> NodeName -> NodeName -> Maybe Hint -> Either ApiRequestError Relationship
findRel schema allRels origin target hint =
  case rel of
    []  -> Left $ NoRelBetween origin target schema
    [r] -> Right r
    -- Here we handle a self reference relationship to not cause a breaking
    -- change: In a self reference we get two relationships with the same
    -- foreign key and relTable/relFtable but with different
    -- cardinalities(m2o/o2m) We output the O2M rel, the M2O rel can be
    -- obtained by using the origin column as an embed hint.
    rs@[rel0, rel1]  -> case (relCardinality rel0, relCardinality rel1, relTable rel0 == relTable rel1 && relForeignTable rel0 == relForeignTable rel1) of
      (O2M cons1, M2O cons2, True) -> if cons1 == cons2 then Right rel0 else Left $ AmbiguousRelBetween origin target rs
      (M2O cons1, O2M cons2, True) -> if cons1 == cons2 then Right rel1 else Left $ AmbiguousRelBetween origin target rs
      _                            -> Left $ AmbiguousRelBetween origin target rs
    rs -> Left $ AmbiguousRelBetween origin target rs
  where
    matchFKSingleCol hint_ cols = length cols == 1 && hint_ == (colName <$> head cols)
    matchConstraint tar card = case card of
      O2M cons -> tar == Just cons
      M2O cons -> tar == Just cons
      _        -> False
    matchJunction hint_ card = case card of
      M2M Junction{junTable} -> hint_ == Just (tableName junTable)
      _                      -> False
    rel = filter (
      \Relationship{..} ->
        -- Both relationship ends need to be on the exposed schema
        schema == tableSchema relTable && schema == tableSchema relForeignTable &&
        (
          -- /projects?select=clients(*)
          origin == tableName relTable  &&  -- projects
          target == tableName relForeignTable ||  -- clients

          -- /projects?select=projects_client_id_fkey(*)
          (
            origin == tableName relTable &&              -- projects
            matchConstraint (Just target) relCardinality -- projects_client_id_fkey
          ) ||
          -- /projects?select=client_id(*)
          (
            origin == tableName relTable &&           -- projects
            matchFKSingleCol (Just target) relColumns -- client_id
          )
        ) && (
          isNothing hint || -- hint is optional

          -- /projects?select=clients!projects_client_id_fkey(*)
          matchConstraint hint relCardinality || -- projects_client_id_fkey

          -- /projects?select=clients!client_id(*) or /projects?select=clients!id(*)
          matchFKSingleCol hint relColumns  || -- client_id
          matchFKSingleCol hint relForeignColumns || -- id

          -- /users?select=tasks!users_tasks(*) many-to-many between users and tasks
          matchJunction hint relCardinality -- users_tasks
        )
      ) allRels

-- previousAlias is only used for the case of self joins
addJoinConditions :: Maybe Alias -> ReadRequest -> Either ApiRequestError ReadRequest
addJoinConditions previousAlias (Node node@(query@Select{from=tbl}, nodeProps@(_, rel, _, _, _, depth)) forest) =
  case rel of
    Just r@Relationship{relCardinality=M2M Junction{junTable}} ->
      let rq = augmentQuery r in
      Node (rq{implicitJoins=tableQi junTable:implicitJoins rq}, nodeProps) <$> updatedForest
    Just r -> Node (augmentQuery r, nodeProps) <$> updatedForest
    Nothing -> Node node <$> updatedForest
  where
    newAlias = case Relationship.isSelfReference <$> rel of
      Just True
        | depth /= 0 -> Just (qiName tbl <> "_" <> show depth) -- root node doesn't get aliased
        | otherwise  -> Nothing
      _              -> Nothing
    augmentQuery r =
      foldr
        (\jc rq@Select{joinConditions=jcs} -> rq{joinConditions=jc:jcs})
        query{fromAlias=newAlias}
        (getJoinConditions previousAlias newAlias r)
    updatedForest = addJoinConditions newAlias `traverse` forest

-- previousAlias and newAlias are used in the case of self joins
getJoinConditions :: Maybe Alias -> Maybe Alias -> Relationship -> [JoinCondition]
getJoinConditions previousAlias newAlias (Relationship Table{tableSchema=tSchema, tableName=tN} cols Table{tableName=ftN} fCols card) =
  case card of
    M2M (Junction Table{tableName=jtn} _ jc1 _ jc2) ->
      zipWith (toJoinCondition tN jtn) cols jc1 ++ zipWith (toJoinCondition ftN jtn) fCols jc2
    _ ->
      zipWith (toJoinCondition tN ftN) cols fCols
  where
    toJoinCondition :: Text -> Text -> Column -> Column -> JoinCondition
    toJoinCondition tb ftb c fc =
      let qi1 = removeSourceCTESchema tSchema tb
          qi2 = removeSourceCTESchema tSchema ftb in
        JoinCondition (maybe qi1 (QualifiedIdentifier mempty) previousAlias, colName c)
                      (maybe qi2 (QualifiedIdentifier mempty) newAlias, colName fc)

    -- On mutation and calling proc cases we wrap the target table in a WITH
    -- {sourceCTEName} if this happens remove the schema `FROM
    -- "schema"."{sourceCTEName}"` and use only the `FROM "{sourceCTEName}"`.
    -- If the schema remains the FROM would be invalid.
    removeSourceCTESchema :: Schema -> TableName -> QualifiedIdentifier
    removeSourceCTESchema schema tbl = QualifiedIdentifier (if tbl == decodeUtf8 sourceCTEName then mempty else schema) tbl

addFilters :: ApiRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addFilters ApiRequest{..} rReq =
  foldr addFilterToNode (Right rReq) flts
  where
    QueryParams.QueryParams{..} = iQueryParams
    flts =
      case iAction of
        ActionInvoke InvGet  -> qsFilters
        ActionInvoke InvHead -> qsFilters
        ActionInvoke _       -> qsFilters
        ActionRead _         -> qsFilters
        _                    -> qsFiltersNotRoot

    addFilterToNode :: (EmbedPath, Filter) -> Either ApiRequestError ReadRequest ->  Either ApiRequestError ReadRequest
    addFilterToNode =
      updateNode (\flt (Node (q@Select {where_=lf}, i) f) -> Node (q{where_=addFilterToLogicForest flt lf}::ReadQuery, i) f)

addOrders :: ApiRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addOrders ApiRequest{..} rReq =
  foldr addOrderToNode (Right rReq) qsOrder
  where
    QueryParams.QueryParams{..} = iQueryParams

    addOrderToNode :: (EmbedPath, [OrderTerm]) -> Either ApiRequestError ReadRequest -> Either ApiRequestError ReadRequest
    addOrderToNode = updateNode (\o (Node (q,i) f) -> Node (q{order=o}, i) f)

addRanges :: ApiRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addRanges ApiRequest{..} rReq =
  case iAction of
    ActionMutate _ -> Right rReq
    _              -> foldr addRangeToNode (Right rReq) =<< ranges
  where
    ranges :: Either ApiRequestError [(EmbedPath, NonnegRange)]
    ranges = first QueryParamError $ QueryParams.pRequestRange `traverse` M.toList iRange

    addRangeToNode :: (EmbedPath, NonnegRange) -> Either ApiRequestError ReadRequest -> Either ApiRequestError ReadRequest
    addRangeToNode = updateNode (\r (Node (q,i) f) -> Node (q{range_=r}, i) f)

addLogicTrees :: ApiRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addLogicTrees ApiRequest{..} rReq =
  foldr addLogicTreeToNode (Right rReq) qsLogic
  where
    QueryParams.QueryParams{..} = iQueryParams

    addLogicTreeToNode :: (EmbedPath, LogicTree) -> Either ApiRequestError ReadRequest -> Either ApiRequestError ReadRequest
    addLogicTreeToNode = updateNode (\t (Node (q@Select{where_=lf},i) f) -> Node (q{where_=t:lf}::ReadQuery, i) f)

-- Find a Node of the Tree and apply a function to it
updateNode :: (a -> ReadRequest -> ReadRequest) -> (EmbedPath, a) -> Either ApiRequestError ReadRequest -> Either ApiRequestError ReadRequest
updateNode f ([], a) rr = f a <$> rr
updateNode _ _ (Left e) = Left e
updateNode f (targetNodeName:remainingPath, a) (Right (Node rootNode forest)) =
  case findNode of
    Nothing -> Left $ NotEmbedded targetNodeName
    Just target ->
      (\node -> Node rootNode $ node : delete target forest) <$>
      updateNode f (remainingPath, a) (Right target)
  where
    findNode :: Maybe ReadRequest
    findNode = find (\(Node (_,(nodeName,_,alias,_,_, _)) _) -> nodeName == targetNodeName || alias == Just targetNodeName) forest

mutateRequest :: Mutation -> Schema -> TableName -> ApiRequest -> [FieldName] -> ReadRequest -> Either Error MutateRequest
mutateRequest mutation schema tName ApiRequest{..} pkCols readReq = mapLeft ApiRequestError $
  case mutation of
    MutationCreate ->
      Right $ Insert qi iColumns body ((,) <$> iPreferResolution <*> Just confCols) [] returnings
    MutationUpdate -> Right $ Update qi iColumns body combinedLogic (iTopLevelRange, pkCols) returnings
    MutationSingleUpsert ->
        if null qsLogic &&
           qsFilterFields == S.fromList pkCols &&
           not (null (S.fromList pkCols)) &&
           all (\case
              Filter _ (OpExpr False (Op OpEqual _)) -> True
              _                                      -> False) filters
          then Right $ Insert qi iColumns body (Just (MergeDuplicates, pkCols)) combinedLogic returnings
        else
          Left InvalidFilters
    MutationDelete -> Right $ Delete qi combinedLogic (iTopLevelRange, pkCols) returnings
  where
    confCols = fromMaybe pkCols qsOnConflict
    QueryParams.QueryParams{..} = iQueryParams
    qi = QualifiedIdentifier schema tName
    returnings =
      if iPreferRepresentation == None
        then []
        else returningCols readReq pkCols
    -- update/delete filters can be only on the root table
    filters = map snd qsFiltersRoot
    logic = map snd qsLogic
    combinedLogic = foldr addFilterToLogicForest logic filters
    body = payRaw <$> iPayload -- the body is assumed to be json at this stage(ApiRequest validates)

callRequest :: ProcDescription -> ApiRequest -> ReadRequest -> CallRequest
callRequest proc apiReq readReq = FunctionCall {
  funCQi = QualifiedIdentifier (pdSchema proc) (pdName proc)
, funCParams = callParams
, funCArgs = payRaw <$> iPayload apiReq
, funCScalar = procReturnsScalar proc
, funCMultipleCall = iPreferParameters apiReq == Just MultipleObjects
, funCReturning = returningCols readReq []
}
  where
    paramsAsSingleObject = iPreferParameters apiReq == Just SingleObject
    callParams = case pdParams proc of
      [prm] | paramsAsSingleObject -> OnePosParam prm
            | ppName prm == mempty -> OnePosParam prm
            | otherwise            -> KeyParams $ specifiedParams [prm]
      prms  -> KeyParams $ specifiedParams prms
    specifiedParams params = filter (\x -> ppName x `S.member` iColumns apiReq) params

returningCols :: ReadRequest -> [FieldName] -> [FieldName]
returningCols rr@(Node _ forest) pkCols
  -- if * is part of the select, we must not add pk or fk columns manually -
  -- otherwise those would be selected and output twice
  | "*" `elem` fldNames = ["*"]
  | otherwise           = returnings
  where
    fldNames = fstFieldNames rr
    -- Without fkCols, when a mutateRequest to
    -- /projects?select=name,clients(name) occurs, the RETURNING SQL part would
    -- be `RETURNING name`(see QueryBuilder).  This would make the embedding
    -- fail because the following JOIN would need the "client_id" column from
    -- projects.  So this adds the foreign key columns to ensure the embedding
    -- succeeds, result would be `RETURNING name, client_id`.
    fkCols = concat $ mapMaybe (\case
        Node (_, (_, Just Relationship{relColumns=cols}, _, _, _, _)) _ -> Just cols
        _                                                        -> Nothing
      ) forest
    -- However if the "client_id" is present, e.g. mutateRequest to
    -- /projects?select=client_id,name,clients(name) we would get `RETURNING
    -- client_id, name, client_id` and then we would produce the "column
    -- reference \"client_id\" is ambiguous" error from PostgreSQL. So we
    -- deduplicate with Set: We are adding the primary key columns as well to
    -- make sure, that a proper location header can always be built for
    -- INSERT/POST
    returnings = S.toList . S.fromList $ fldNames ++ (colName <$> fkCols) ++ pkCols

-- Traditional filters(e.g. id=eq.1) are added as root nodes of the LogicTree
-- they are later concatenated with AND in the QueryBuilder
addFilterToLogicForest :: Filter -> [LogicTree] -> [LogicTree]
addFilterToLogicForest flt lf = Stmnt flt : lf
