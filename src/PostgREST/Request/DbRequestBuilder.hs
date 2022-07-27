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

import qualified Data.HashMap.Strict as HM
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
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.Error                    (Error (..))
import PostgREST.Query.SqlFragment        (sourceCTEName)
import PostgREST.RangeQuery               (NonnegRange, allRange,
                                           restrictRange)
import PostgREST.Request.ApiRequest       (Action (..),
                                           ApiRequest (..),
                                           InvokeMethod (..),
                                           Mutation (..),
                                           Payload (..))

import PostgREST.Request.MutateQuery
import PostgREST.Request.Preferences
import PostgREST.Request.ReadQuery   as ReadQuery
import PostgREST.Request.Types

import qualified PostgREST.Request.QueryParams as QueryParams

import Protolude hiding (from)

-- | Builds the ReadRequest tree on a number of stages.
-- | Adds filters, order, limits on its respective nodes.
-- | Adds joins conditions obtained from resource embedding.
readRequest :: Schema -> TableName -> Maybe Integer -> RelationshipsMap -> ApiRequest -> Either Error ReadRequest
readRequest schema rootTableName maxRows allRels apiRequest  =
  mapLeft ApiRequestError $
  treeRestrictRange maxRows (iAction apiRequest) =<<
  augmentRequestWithJoin schema allRels =<<
  addLogicTrees apiRequest =<<
  addRanges apiRequest =<<
  addOrders apiRequest =<<
  addFilters apiRequest (initReadRequest rootName rootAlias qsSelect)
  where
    QueryParams.QueryParams{..} = iQueryParams apiRequest
    (rootName, rootAlias) = case iAction apiRequest of
      ActionRead _ -> (QualifiedIdentifier schema rootTableName, Nothing)
      -- the CTE we use for non-read cases has a sourceCTEName(see Statements.hs) as the WITH name so we use the table name as an alias so findRel can find the right relationship
      _ -> (QualifiedIdentifier mempty $ decodeUtf8 sourceCTEName, Just rootTableName)

-- Build the initial tree with a Depth attribute so when a self join occurs we
-- can differentiate the parent and child tables by having an alias like
-- "table_depth", this is related to
-- http://github.com/PostgREST/postgrest/issues/987.
initReadRequest :: QualifiedIdentifier -> Maybe Alias -> [Tree SelectItem] -> ReadRequest
initReadRequest rootQi rootAlias =
  foldr (treeEntry rootDepth) initial
  where
    rootDepth = 0
    rootSchema = qiSchema rootQi
    rootName = qiName rootQi
    initial = Node (Select [] rootQi rootAlias [] [] [] [] allRange, (rootName, Nothing, Nothing, Nothing, Nothing, rootDepth)) []
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

augmentRequestWithJoin :: Schema -> RelationshipsMap -> ReadRequest -> Either ApiRequestError ReadRequest
augmentRequestWithJoin schema allRels request =
  addRels schema allRels Nothing request
  >>= addJoinConditions Nothing

addRels :: Schema -> RelationshipsMap -> Maybe ReadRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addRels schema allRels parentNode (Node (query@Select{from=tbl}, (nodeName, _, alias, hint, joinType, depth)) forest) =
  case parentNode of
    Just (Node (Select{from=parentNodeQi, fromAlias=aliasQi}, _) _) ->
      let newFrom r = if qiName tbl == nodeName then relForeignTable r else tbl
          newReadNode = (\r -> (query{from=newFrom r}, (nodeName, Just r, alias, hint, joinType, depth))) <$> rel
          origin = if depth == 1 -- Only on depth 1 we check if the parent(depth 0) has an alias so the sourceCTEName alias can be found as a relationship
            then fromMaybe (qiName parentNodeQi) aliasQi
            else qiName parentNodeQi
          rel = findRel schema allRels origin nodeName hint
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
-- /origin?select=target!hint(*). The origin can be a table or view.
findRel :: Schema -> RelationshipsMap -> NodeName -> NodeName -> Maybe Hint -> Either ApiRequestError Relationship
findRel schema allRels origin target hint =
  case rels of
    []  -> Left $ NoRelBetween origin target schema
    [r] -> Right r
    rs  -> Left $ AmbiguousRelBetween origin target rs
  where
    matchFKSingleCol hint_ card = case card of
      O2M _ [(col, _)] -> hint_ == col
      M2O _ [(col, _)] -> hint_ == col
      _                -> False
    matchFKRefSingleCol hint_ card  = case card of
      O2M _ [(_, fCol)] -> hint_ == fCol
      M2O _ [(_, fCol)] -> hint_ == fCol
      _                 -> False
    matchConstraint tar card = case card of
      O2M cons _ -> tar == cons
      M2O cons _ -> tar == cons
      _          -> False
    matchJunction hint_ card = case card of
      M2M Junction{junTable} -> hint_ == qiName junTable
      _                      -> False
    isM2O card = case card of
      M2O _ _ -> True
      _       -> False
    isO2M card = case card of
      O2M _ _ -> True
      _       -> False
    rels = filter (
      \Relationship{..} ->
        -- In a self-relationship we have a single foreign key but two relationships with different cardinalities: M2O/O2M. For disambiguation, we use the convention of getting:
        -- TODO: handle one-to-one and many-to-many self-relationships
        if relIsSelf
        then case hint of
          Nothing ->
            -- The O2M by using the table name in the target
            target == qiName relForeignTable && isO2M relCardinality -- /family_tree?select=children:family_tree(*)
            ||
            -- The M2O by using the column name in the target
            matchFKSingleCol target relCardinality && isM2O relCardinality -- /family_tree?select=parent(*)
          Just hnt ->
            -- /organizations?select=auditees:organizations!auditor(*)
            target == qiName relForeignTable && isO2M relCardinality
            && matchFKRefSingleCol hnt relCardinality -- auditor
        else case hint of
          -- target = table / view / constraint / column-from-origin (constraint/column-from-origin can only come from tables https://github.com/PostgREST/postgrest/issues/2277)
          -- hint   = table / view / constraint / column-from-origin / column-from-target (hint can take table / view values to aid in finding the junction in an m2m relationship)
          Nothing ->
              -- /projects?select=clients(*)
              target == qiName relForeignTable -- clients
              ||
              -- /projects?select=projects_client_id_fkey(*)
              matchConstraint target relCardinality -- projects_client_id_fkey
              && not relFTableIsView
              ||
              -- /projects?select=client_id(*)
              matchFKSingleCol target relCardinality -- client_id
              && not relFTableIsView
          Just hnt ->
            -- /projects?select=clients(*)
            target == qiName relForeignTable -- clients
            && (
              -- /projects?select=clients!projects_client_id_fkey(*)
              matchConstraint hnt relCardinality || -- projects_client_id_fkey

              -- /projects?select=clients!client_id(*) or /projects?select=clients!id(*)
              matchFKSingleCol hnt relCardinality      || -- client_id
              matchFKRefSingleCol hnt relCardinality   || -- id

              -- /users?select=tasks!users_tasks(*) many-to-many between users and tasks
              matchJunction hnt relCardinality -- users_tasks
            )
      ) $ fromMaybe mempty $ HM.lookup (QualifiedIdentifier schema origin, schema) allRels

-- previousAlias is only used for the case of self joins
addJoinConditions :: Maybe Alias -> ReadRequest -> Either ApiRequestError ReadRequest
addJoinConditions previousAlias (Node node@(query@Select{from=tbl,fromAlias=tblAlias}, nodeProps@(_, rel, _, _, _, depth)) forest) =
  case rel of
    Just r@Relationship{relCardinality=M2M Junction{junTable}} ->
      let rq = augmentQuery r in
      Node (rq{implicitJoins=junTable:implicitJoins rq}, nodeProps) <$> updatedForest
    Just r -> Node (augmentQuery r, nodeProps) <$> updatedForest
    Nothing -> Node node <$> updatedForest
  where
    newAlias = if depth == 0
      then tblAlias -- only use the alias on the root node(depth 0) for when the sourceCTEName alias is used for joining
      else case relIsSelf <$> rel of -- no need to apply the self reference alias on depth 0 only on the next depths
        Just True -> Just (qiName tbl <> "_" <> show depth)
        _         -> Nothing
    augmentQuery r =
      foldr
        (\jc rq@Select{joinConditions=jcs} -> rq{joinConditions=jc:jcs})
        query{fromAlias=newAlias}
        (getJoinConditions previousAlias newAlias r)
    updatedForest = addJoinConditions newAlias `traverse` forest

-- previousAlias and newAlias are used in the case of self joins
getJoinConditions :: Maybe Alias -> Maybe Alias -> Relationship -> [JoinCondition]
getJoinConditions previousAlias newAlias (Relationship QualifiedIdentifier{qiSchema=tSchema, qiName=tN} QualifiedIdentifier{qiName=ftN} _ card _ _) =
  case card of
    M2M (Junction QualifiedIdentifier{qiName=jtn} _ _ jcols1 jcols2) ->
      (toJoinCondition previousAlias newAlias tN jtn <$> jcols1) ++ (toJoinCondition Nothing Nothing ftN jtn <$> jcols2)
    O2M _ cols ->
      toJoinCondition previousAlias newAlias tN ftN <$> cols
    M2O _ cols ->
      toJoinCondition previousAlias newAlias tN ftN <$> cols
  where
    toJoinCondition :: Maybe Alias -> Maybe Alias -> Text -> Text -> (FieldName, FieldName) -> JoinCondition
    toJoinCondition prAl newAl tb ftb (c, fc) =
      let qi1 = QualifiedIdentifier tSchema tb
          qi2 = QualifiedIdentifier tSchema ftb in
        JoinCondition (maybe qi1 (QualifiedIdentifier mempty) prAl, c)
                      (maybe qi2 (QualifiedIdentifier mempty) newAl, fc)

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
      updateNode (\flt (Node (q@Select {where_=lf}, i) f) -> Node (q{ReadQuery.where_=addFilterToLogicForest flt lf}, i) f)

addOrders :: ApiRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addOrders ApiRequest{..} rReq =
  case iAction of
    ActionMutate _ -> Right rReq
    _              -> foldr addOrderToNode (Right rReq) qsOrder
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
    ranges = first QueryParamError $ QueryParams.pRequestRange `traverse` HM.toList iRange

    addRangeToNode :: (EmbedPath, NonnegRange) -> Either ApiRequestError ReadRequest -> Either ApiRequestError ReadRequest
    addRangeToNode = updateNode (\r (Node (q,i) f) -> Node (q{range_=r}, i) f)

addLogicTrees :: ApiRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addLogicTrees ApiRequest{..} rReq =
  foldr addLogicTreeToNode (Right rReq) qsLogic
  where
    QueryParams.QueryParams{..} = iQueryParams

    addLogicTreeToNode :: (EmbedPath, LogicTree) -> Either ApiRequestError ReadRequest -> Either ApiRequestError ReadRequest
    addLogicTreeToNode = updateNode (\t (Node (q@Select{where_=lf},i) f) -> Node (q{ReadQuery.where_=t:lf}, i) f)

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
    MutationUpdate -> Right $ Update qi iColumns body combinedLogic pkCols iTopLevelRange rootOrder returnings
    MutationSingleUpsert ->
        if null qsLogic &&
           qsFilterFields == S.fromList pkCols &&
           not (null (S.fromList pkCols)) &&
           all (\case
              Filter _ (OpExpr False (Op OpEqual _)) -> True
              _                                      -> False) qsFiltersRoot
          then Right $ Insert qi iColumns body (Just (MergeDuplicates, pkCols)) combinedLogic returnings
        else
          Left InvalidFilters
    MutationDelete
        | noBodyFilterLimit -> Left $ NoBodyFilterLimit "delete"
        | otherwise         -> Right $ Delete qi deleteCols body combinedLogic iTopLevelRange rootOrder returnings

  where
    confCols = fromMaybe pkCols qsOnConflict
    QueryParams.QueryParams{..} = iQueryParams
    qi = QualifiedIdentifier schema tName
    returnings =
      if iPreferRepresentation == None
        then []
        else returningCols readReq pkCols
    logic = map snd qsLogic
    rootOrder = maybe [] snd $ find (\(x, _) -> null x) qsOrder
    combinedLogic = foldr addFilterToLogicForest logic qsFiltersRoot
    body = payRaw <$> iPayload -- the body is assumed to be json at this stage(ApiRequest validates)
    payloadHasPks = all (`elem` iColumns) pkCols
    noBodyFilterLimit = null qsColumns && null qsLogic && null qsFilters && null qsRanges && (isNothing body || not payloadHasPks)
    deleteCols = if null qsColumns then S.fromList pkCols else iColumns

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
    specifiedParams = filter (\x -> ppName x `S.member` iColumns apiReq)

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
        Node (_, (_, Just Relationship{relCardinality=O2M _ cols}, _, _, _, _)) _ -> Just $ fst <$> cols
        Node (_, (_, Just Relationship{relCardinality=M2O _ cols}, _, _, _, _)) _ -> Just $ fst <$> cols
        Node (_, (_, Just Relationship{relCardinality=M2M Junction{junColumns1, junColumns2}}, _, _, _, _)) _ -> Just $ (fst <$> junColumns1) ++ (fst <$> junColumns2)
        _                                                        -> Nothing
      ) forest
    -- However if the "client_id" is present, e.g. mutateRequest to
    -- /projects?select=client_id,name,clients(name) we would get `RETURNING
    -- client_id, name, client_id` and then we would produce the "column
    -- reference \"client_id\" is ambiguous" error from PostgreSQL. So we
    -- deduplicate with Set: We are adding the primary key columns as well to
    -- make sure, that a proper location header can always be built for
    -- INSERT/POST
    returnings = S.toList . S.fromList $ fldNames ++ fkCols ++ pkCols

-- Traditional filters(e.g. id=eq.1) are added as root nodes of the LogicTree
-- they are later concatenated with AND in the QueryBuilder
addFilterToLogicForest :: Filter -> [LogicTree] -> [LogicTree]
addFilterToLogicForest flt lf = Stmnt flt : lf
