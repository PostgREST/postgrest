{-|
Module      : PostgREST.Plan
Description : PostgREST Request Planner

This module is in charge of building an intermediate
representation between the HTTP request and the
final resulting SQL query.

A query tree is built in case of resource embedding. By inferring the
relationship between tables, join conditions are added for every embedded
resource.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module PostgREST.Plan
  ( readPlan
  , mutateReadPlan
  , callReadPlan
  , MutateReadPlan(..)
  , CallReadPlan(..)
  ) where

import qualified Data.HashMap.Strict        as HM
import qualified Data.Set                   as S
import qualified PostgREST.DbStructure.Proc as Proc

import Data.Either.Combinators (mapLeft)
import Data.List               (delete)
import Data.Tree               (Tree (..))

import PostgREST.ApiRequest               (Action (..),
                                           ApiRequest (..),
                                           InvokeMethod (..),
                                           Mutation (..),
                                           Payload (..))
import PostgREST.Config                   (AppConfig (..))
import PostgREST.DbStructure              (DbStructure (..))
import PostgREST.DbStructure.Identifiers  (FieldName,
                                           QualifiedIdentifier (..),
                                           Schema)
import PostgREST.DbStructure.Proc         (ProcDescription (..),
                                           ProcParam (..),
                                           procReturnsScalar)
import PostgREST.DbStructure.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.DbStructure.Table        (tablePKCols)
import PostgREST.Error                    (Error (..))
import PostgREST.Query.SqlFragment        (sourceCTEName)
import PostgREST.RangeQuery               (NonnegRange, allRange,
                                           restrictRange)

import PostgREST.Plan.CallPlan
import PostgREST.Plan.MutatePlan
import PostgREST.Plan.ReadPlan   as ReadPlan

import PostgREST.ApiRequest.Preferences
import PostgREST.ApiRequest.Types

import qualified PostgREST.ApiRequest.QueryParams as QueryParams

import Protolude hiding (from)

data MutateReadPlan = MutateReadPlan {
  mrReadPlan   :: ReadPlanTree
, mrMutatePlan :: MutatePlan
}

data CallReadPlan = CallReadPlan {
  crReadPlan :: ReadPlanTree
, crCallPlan :: CallPlan
}

mutateReadPlan :: Mutation -> ApiRequest -> QualifiedIdentifier -> AppConfig -> DbStructure -> Either Error MutateReadPlan
mutateReadPlan  mutation apiRequest identifier conf dbStructure = do
  rPlan <- readPlan identifier conf dbStructure apiRequest
  mPlan <- mutatePlan mutation identifier apiRequest dbStructure rPlan
  return $ MutateReadPlan rPlan mPlan

callReadPlan :: ProcDescription -> AppConfig -> DbStructure -> ApiRequest -> Either Error CallReadPlan
callReadPlan proc conf dbStructure apiRequest = do
  let identifier = QualifiedIdentifier (pdSchema proc) (fromMaybe (pdName proc) $ Proc.procTableName proc)
  rPlan <- readPlan identifier conf dbStructure apiRequest
  let cPlan = callPlan proc apiRequest rPlan
  return $ CallReadPlan rPlan cPlan

-- | Builds the ReadPlan tree on a number of stages.
-- | Adds filters, order, limits on its respective nodes.
-- | Adds joins conditions obtained from resource embedding.
readPlan :: QualifiedIdentifier -> AppConfig -> DbStructure -> ApiRequest -> Either Error ReadPlanTree
readPlan qi@QualifiedIdentifier{..} AppConfig{configDbMaxRows} DbStructure{dbRelationships} apiRequest  =
  mapLeft ApiRequestError $
  treeRestrictRange configDbMaxRows (iAction apiRequest) =<<
  augmentRequestWithJoin qiSchema dbRelationships =<<
  addLogicTrees apiRequest =<<
  addRanges apiRequest =<<
  addOrders apiRequest =<<
  addFilters apiRequest (initReadRequest rootName rootAlias qsSelect)
  where
    QueryParams.QueryParams{..} = iQueryParams apiRequest
    (rootName, rootAlias) = case iAction apiRequest of
      ActionRead _ -> (qi, Nothing)
      -- the CTE we use for non-read cases has a sourceCTEName(see Statements.hs) as the WITH name so we use the table name as an alias so findRel can find the right relationship
      _ -> (QualifiedIdentifier mempty $ decodeUtf8 sourceCTEName, Just qiName)

-- Build the initial tree with a Depth attribute so when a self join occurs we
-- can differentiate the parent and child tables by having an alias like
-- "table_depth", this is related to
-- http://github.com/PostgREST/postgrest/issues/987.
initReadRequest :: QualifiedIdentifier -> Maybe Alias -> [Tree SelectItem] -> ReadPlanTree
initReadRequest rootQi rootAlias =
  foldr (treeEntry rootDepth) initial
  where
    rootDepth = 0
    rootSchema = qiSchema rootQi
    rootName = qiName rootQi
    initial = Node (ReadPlan [] rootQi rootAlias [] [] [] allRange rootName Nothing Nothing Nothing Nothing rootDepth) []
    treeEntry :: Depth -> Tree SelectItem -> ReadPlanTree -> ReadPlanTree
    treeEntry depth (Node fld@((fn, _),_,alias, hint, joinType) fldForest) (Node q rForest) =
      let nxtDepth = succ depth in
      case fldForest of
        [] -> Node q{select=fld:select q} rForest
        _  -> Node q $
              foldr (treeEntry nxtDepth)
              (Node (ReadPlan [] (QualifiedIdentifier rootSchema fn) Nothing [] [] [] allRange fn Nothing alias hint joinType nxtDepth) [])
              fldForest:rForest

-- | Enforces the `max-rows` config on the result
treeRestrictRange :: Maybe Integer -> Action -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
treeRestrictRange _ (ActionMutate _) request = Right request
treeRestrictRange maxRows _ request = pure $ nodeRestrictRange maxRows <$> request
  where
    nodeRestrictRange :: Maybe Integer -> ReadPlan -> ReadPlan
    nodeRestrictRange m q@ReadPlan{range_=r} = q{range_=restrictRange m r }

augmentRequestWithJoin :: Schema -> RelationshipsMap -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
augmentRequestWithJoin schema allRels request =
 addJoinConditions Nothing <$> addRels schema allRels Nothing request

addRels :: Schema -> RelationshipsMap -> Maybe ReadPlanTree -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRels schema allRels parentNode (Node query@ReadPlan{from=tbl,nodeName,nodeHint,nodeDepth} forest) =
  case parentNode of
    Just (Node ReadPlan{from=parentNodeQi, fromAlias=aliasQi} _) ->
      let newFrom r = if qiName tbl == nodeName then relForeignTable r else tbl
          newReadPlan = (\r ->
            if not $ relIsSelf r -- add alias if self rel TODO consolidate aliasing in another function
              then query{from=newFrom r, nodeRel=Just r}
              else query{from=newFrom r, nodeRel=Just r, fromAlias=Just (qiName (newFrom r) <> "_" <> show nodeDepth)}
            ) <$> rel
          origin = if nodeDepth == 1 -- Only on depth 1 we check if the root(depth 0) has an alias so the sourceCTEName alias can be found as a relationship
            then fromMaybe (qiName parentNodeQi) aliasQi
            else qiName parentNodeQi
          rel = findRel schema allRels origin nodeName nodeHint
      in
      Node <$> newReadPlan <*> (updateForest . hush $ Node <$> newReadPlan <*> pure forest)
    _ ->
      Node query <$> updateForest (Just $ Node query forest)
  where
    updateForest :: Maybe ReadPlanTree -> Either ApiRequestError [ReadPlanTree]
    updateForest rq = addRels schema allRels rq `traverse` forest

-- applies aliasing to join conditions TODO refactor, this should go into the querybuilder module
addJoinConditions :: Maybe Alias -> ReadPlanTree -> ReadPlanTree
addJoinConditions _ (Node node@ReadPlan{fromAlias=tblAlias, nodeRel=Nothing} forest) = Node node (addJoinConditions tblAlias <$> forest)
addJoinConditions _ (Node node@ReadPlan{fromAlias=tblAlias, nodeRel=Just ComputedRelationship{}} forest) = Node node (addJoinConditions tblAlias <$> forest)
addJoinConditions previousAlias (Node query@ReadPlan{fromAlias=tblAlias, nodeRel=Just Relationship{relTable=qi,relForeignTable=fQi,relCardinality=card}} forest) =
  Node query{joinConditions=joinConds} (addJoinConditions tblAlias <$> forest)
  where
    QualifiedIdentifier{qiSchema=tSchema, qiName=tN} = qi
    QualifiedIdentifier{qiName=ftN} = fQi
    joinConds =
      case card of
        M2M (Junction QualifiedIdentifier{qiName=jtn} _ _ jcols1 jcols2) ->
           (toJoinCondition Nothing Nothing ftN jtn <$> jcols2) ++ (toJoinCondition previousAlias tblAlias tN jtn <$> jcols1)
        O2M _ cols ->
          toJoinCondition previousAlias tblAlias tN ftN <$> cols
        M2O _ cols ->
          toJoinCondition previousAlias tblAlias tN ftN <$> cols
        O2O _ cols ->
          toJoinCondition previousAlias tblAlias tN ftN <$> cols
    toJoinCondition :: Maybe Alias -> Maybe Alias -> Text -> Text -> (FieldName, FieldName) -> JoinCondition
    toJoinCondition prAl newAl tb ftb (c, fc) =
      let qi1 = QualifiedIdentifier tSchema ftb
          qi2 = QualifiedIdentifier tSchema tb in
        JoinCondition (maybe qi1 (QualifiedIdentifier mempty) newAl, fc)
                      (maybe qi2 (QualifiedIdentifier mempty) prAl, c)

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
      O2O _ [(col, _)] -> hint_ == col
      _                -> False
    matchFKRefSingleCol hint_ card  = case card of
      O2M _ [(_, fCol)] -> hint_ == fCol
      M2O _ [(_, fCol)] -> hint_ == fCol
      O2O _ [(_, fCol)] -> hint_ == fCol
      _                 -> False
    matchConstraint tar card = case card of
      O2M cons _ -> tar == cons
      M2O cons _ -> tar == cons
      O2O cons _ -> tar == cons
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
    rels = filter (\case
      ComputedRelationship{relFunction} -> target == qiName relFunction
      Relationship{..} ->
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

addFilters :: ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
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

    addFilterToNode :: (EmbedPath, Filter) -> Either ApiRequestError ReadPlanTree ->  Either ApiRequestError ReadPlanTree
    addFilterToNode =
      updateNode (\flt (Node q@ReadPlan{where_=lf} f) -> Node q{ReadPlan.where_=addFilterToLogicForest flt lf}  f)

addOrders :: ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addOrders ApiRequest{..} rReq =
  case iAction of
    ActionMutate _ -> Right rReq
    _              -> foldr addOrderToNode (Right rReq) qsOrder
  where
    QueryParams.QueryParams{..} = iQueryParams

    addOrderToNode :: (EmbedPath, [OrderTerm]) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
    addOrderToNode = updateNode (\o (Node q f) -> Node q{order=o} f)

addRanges :: ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRanges ApiRequest{..} rReq =
  case iAction of
    ActionMutate _ -> Right rReq
    _              -> foldr addRangeToNode (Right rReq) =<< ranges
  where
    ranges :: Either ApiRequestError [(EmbedPath, NonnegRange)]
    ranges = first QueryParamError $ QueryParams.pRequestRange `traverse` HM.toList iRange

    addRangeToNode :: (EmbedPath, NonnegRange) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
    addRangeToNode = updateNode (\r (Node q f) -> Node q{range_=r} f)

addLogicTrees :: ApiRequest -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addLogicTrees ApiRequest{..} rReq =
  foldr addLogicTreeToNode (Right rReq) qsLogic
  where
    QueryParams.QueryParams{..} = iQueryParams

    addLogicTreeToNode :: (EmbedPath, LogicTree) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
    addLogicTreeToNode = updateNode (\t (Node q@ReadPlan{where_=lf} f) -> Node q{ReadPlan.where_=t:lf} f)

-- Find a Node of the Tree and apply a function to it
updateNode :: (a -> ReadPlanTree -> ReadPlanTree) -> (EmbedPath, a) -> Either ApiRequestError ReadPlanTree -> Either ApiRequestError ReadPlanTree
updateNode f ([], a) rr = f a <$> rr
updateNode _ _ (Left e) = Left e
updateNode f (targetNodeName:remainingPath, a) (Right (Node rootNode forest)) =
  case findNode of
    Nothing -> Left $ NotEmbedded targetNodeName
    Just target ->
      (\node -> Node rootNode $ node : delete target forest) <$>
      updateNode f (remainingPath, a) (Right target)
  where
    findNode :: Maybe ReadPlanTree
    findNode = find (\(Node ReadPlan{nodeName, nodeAlias} _) -> nodeName == targetNodeName || nodeAlias == Just targetNodeName) forest

mutatePlan :: Mutation -> QualifiedIdentifier -> ApiRequest -> DbStructure -> ReadPlanTree -> Either Error MutatePlan
mutatePlan mutation qi ApiRequest{..} dbStructure readReq = mapLeft ApiRequestError $
  case mutation of
    MutationCreate ->
      Right $ Insert qi iColumns body ((,) <$> iPreferResolution <*> Just confCols) [] returnings pkCols
    MutationUpdate -> Right $ Update qi iColumns body combinedLogic iTopLevelRange rootOrder returnings
    MutationSingleUpsert ->
        if null qsLogic &&
           qsFilterFields == S.fromList pkCols &&
           not (null (S.fromList pkCols)) &&
           all (\case
              Filter _ (OpExpr False (Op OpEqual _)) -> True
              _                                      -> False) qsFiltersRoot
          then Right $ Insert qi iColumns body (Just (MergeDuplicates, pkCols)) combinedLogic returnings mempty
        else
          Left InvalidFilters
    MutationDelete -> Right $ Delete qi combinedLogic iTopLevelRange rootOrder returnings
  where
    confCols = fromMaybe pkCols qsOnConflict
    QueryParams.QueryParams{..} = iQueryParams
    returnings =
      if iPreferRepresentation == None
        then []
        else returningCols readReq pkCols
    pkCols = maybe mempty tablePKCols $ HM.lookup qi $ dbTables dbStructure
    logic = map snd qsLogic
    rootOrder = maybe [] snd $ find (\(x, _) -> null x) qsOrder
    combinedLogic = foldr addFilterToLogicForest logic qsFiltersRoot
    body = payRaw <$> iPayload -- the body is assumed to be json at this stage(ApiRequest validates)

callPlan :: ProcDescription -> ApiRequest -> ReadPlanTree -> CallPlan
callPlan proc apiReq readReq = FunctionCall {
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

returningCols :: ReadPlanTree -> [FieldName] -> [FieldName]
returningCols rr@(Node _ forest) pkCols
  -- if * is part of the select, we must not add pk or fk columns manually -
  -- otherwise those would be selected and output twice
  | "*" `elem` fldNames = ["*"]
  | otherwise           = returnings
  where
    fldNames = fstFieldNames rr
    -- Without fkCols, when a mutatePlan to
    -- /projects?select=name,clients(name) occurs, the RETURNING SQL part would
    -- be `RETURNING name`(see QueryBuilder).  This would make the embedding
    -- fail because the following JOIN would need the "client_id" column from
    -- projects.  So this adds the foreign key columns to ensure the embedding
    -- succeeds, result would be `RETURNING name, client_id`.
    fkCols = concat $ mapMaybe (\case
        Node ReadPlan{nodeRel=Just Relationship{relCardinality=O2M _ cols}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{nodeRel=Just Relationship{relCardinality=M2O _ cols}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{nodeRel=Just Relationship{relCardinality=O2O _ cols}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{nodeRel=Just Relationship{relCardinality=M2M Junction{junColumns1, junColumns2}}} _ ->
          Just $ (fst <$> junColumns1) ++ (fst <$> junColumns2)
        Node ReadPlan{nodeRel=Just ComputedRelationship{}} _ ->
          Nothing
        Node ReadPlan{nodeRel=Nothing} _ ->
          Nothing
      ) forest
    hasComputedRel = isJust $ find (\case
      Node ReadPlan{nodeRel=Just ComputedRelationship{}} _ -> True
      _                                                    -> False
      ) forest
    -- However if the "client_id" is present, e.g. mutatePlan to
    -- /projects?select=client_id,name,clients(name) we would get `RETURNING
    -- client_id, name, client_id` and then we would produce the "column
    -- reference \"client_id\" is ambiguous" error from PostgreSQL. So we
    -- deduplicate with Set: We are adding the primary key columns as well to
    -- make sure, that a proper location header can always be built for
    -- INSERT/POST
    returnings =
      if not hasComputedRel
        then S.toList . S.fromList $ fldNames ++ fkCols ++ pkCols
        else ["*"] -- on computed relationships we cannot know the required columns for an embedding to succeed, so we just return all

-- Traditional filters(e.g. id=eq.1) are added as root nodes of the LogicTree
-- they are later concatenated with AND in the QueryBuilder
addFilterToLogicForest :: Filter -> [LogicTree] -> [LogicTree]
addFilterToLogicForest flt lf = Stmnt flt : lf
