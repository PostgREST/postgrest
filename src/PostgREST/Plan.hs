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
import qualified PostgREST.SchemaCache.Proc as Proc

import Data.Either.Combinators (mapLeft)
import Data.List               (delete)
import Data.Tree               (Tree (..))

import PostgREST.ApiRequest               (Action (..),
                                           ApiRequest (..),
                                           InvokeMethod (..),
                                           Mutation (..),
                                           Payload (..))
import PostgREST.Config                   (AppConfig (..))
import PostgREST.Error                    (Error (..))
import PostgREST.Query.SqlFragment        (sourceCTEName)
import PostgREST.RangeQuery               (NonnegRange, allRange,
                                           convertToLimitZeroRange,
                                           restrictRange)
import PostgREST.SchemaCache              (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers  (FieldName,
                                           QualifiedIdentifier (..),
                                           Schema)
import PostgREST.SchemaCache.Proc         (ProcDescription (..),
                                           ProcParam (..),
                                           procReturnsScalar)
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           RelationshipsMap,
                                           relIsToOne)
import PostgREST.SchemaCache.Table        (tablePKCols)

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

mutateReadPlan :: Mutation -> ApiRequest -> QualifiedIdentifier -> AppConfig -> SchemaCache -> Either Error MutateReadPlan
mutateReadPlan  mutation apiRequest identifier conf sCache = do
  rPlan <- readPlan identifier conf sCache apiRequest
  mPlan <- mutatePlan mutation identifier apiRequest sCache rPlan
  return $ MutateReadPlan rPlan mPlan

callReadPlan :: ProcDescription -> AppConfig -> SchemaCache -> ApiRequest -> Either Error CallReadPlan
callReadPlan proc conf sCache apiRequest = do
  let identifier = QualifiedIdentifier (pdSchema proc) (fromMaybe (pdName proc) $ Proc.procTableName proc)
  rPlan <- readPlan identifier conf sCache apiRequest
  let cPlan = callPlan proc apiRequest rPlan
  return $ CallReadPlan rPlan cPlan

-- | Builds the ReadPlan tree on a number of stages.
-- | Adds filters, order, limits on its respective nodes.
-- | Adds joins conditions obtained from resource embedding.
readPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> Either Error ReadPlanTree
readPlan qi@QualifiedIdentifier{..} AppConfig{configDbMaxRows} SchemaCache{dbRelationships} apiRequest  =
  mapLeft ApiRequestError $
  treeRestrictRange configDbMaxRows (iAction apiRequest) =<<
  addNullEmbedFilters =<<
  validateSpreadEmbeds =<<
  addRelatedOrders =<<
  addRels qiSchema (iAction apiRequest) dbRelationships Nothing =<<
  addLogicTrees apiRequest =<<
  addRanges apiRequest =<<
  addOrders apiRequest =<<
  addFilters apiRequest (initReadRequest qi $ QueryParams.qsSelect $ iQueryParams apiRequest)

-- Build the initial read plan tree
initReadRequest :: QualifiedIdentifier -> [Tree SelectItem] -> ReadPlanTree
initReadRequest qi@QualifiedIdentifier{..} =
  foldr (treeEntry rootDepth) $ Node defReadPlan{from=qi, relName=qiName, depth=rootDepth} []
  where
    rootDepth = 0
    defReadPlan = ReadPlan [] (QualifiedIdentifier mempty mempty) Nothing [] [] allRange mempty Nothing [] Nothing mempty Nothing Nothing False rootDepth
    treeEntry :: Depth -> Tree SelectItem -> ReadPlanTree -> ReadPlanTree
    treeEntry depth (Node si fldForest) (Node q rForest) =
      let nxtDepth = succ depth in
      case si of
        SelectRelation{..} ->
          Node q $
            foldr (treeEntry nxtDepth)
            (Node defReadPlan{from=QualifiedIdentifier qiSchema selRelation, relName=selRelation, relAlias=selAlias, relHint=selHint, relJoinType=selJoinType, depth=nxtDepth} [])
            fldForest:rForest
        SpreadRelation{..} ->
          Node q $
            foldr (treeEntry nxtDepth)
            (Node defReadPlan{from=QualifiedIdentifier qiSchema selRelation, relName=selRelation, relHint=selHint, relJoinType=selJoinType, depth=nxtDepth, relIsSpread=True} [])
            fldForest:rForest
        SelectField{..} ->
          Node q{select=(selField, selCast, selAlias):select q} rForest

-- | Enforces the `max-rows` config on the result
treeRestrictRange :: Maybe Integer -> Action -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
treeRestrictRange _ (ActionMutate _) request = Right request
treeRestrictRange maxRows _ request = pure $ nodeRestrictRange maxRows <$> request
  where
    nodeRestrictRange :: Maybe Integer -> ReadPlan -> ReadPlan
    nodeRestrictRange m q@ReadPlan{range_=r} = q{range_= convertToLimitZeroRange r (restrictRange m r) }

-- add relationships to the nodes of the tree by traversing the forest while keeping track of the parentNode(https://stackoverflow.com/questions/22721064/get-the-parent-of-a-node-in-data-tree-haskell#comment34627048_22721064)
-- also adds aliasing
addRels :: Schema -> Action -> RelationshipsMap -> Maybe ReadPlanTree -> ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRels schema action allRels parentNode (Node rPlan@ReadPlan{relName,relHint,relAlias,depth} forest) =
  case parentNode of
    Just (Node ReadPlan{from=parentNodeQi, fromAlias=parentAlias} _) ->
      let
        newReadPlan = (\r ->
          let newAlias = Just (qiName (relForeignTable r) <> "_" <> show depth)
              aggAlias = qiName (relTable r) <> "_" <> fromMaybe relName relAlias <> "_" <> show depth in
          case r of
            Relationship{relCardinality=M2M _} -> -- m2m does internal implicit joins that don't need aliasing
              rPlan{from=relForeignTable r, relToParent=Just r, relAggAlias=aggAlias, relJoinConds=getJoinConditions Nothing parentAlias r}
            ComputedRelationship{} ->
              rPlan{from=relForeignTable r, relToParent=Just r{relTable=maybe (relTable r) (QualifiedIdentifier mempty) parentAlias}, relAggAlias=aggAlias, fromAlias=newAlias}
            _ ->
              rPlan{from=relForeignTable r, relToParent=Just r, relAggAlias=aggAlias, fromAlias=newAlias, relJoinConds=getJoinConditions newAlias parentAlias r}
          ) <$> rel
        origin = if depth == 1 -- Only on depth 1 we check if the root(depth 0) has an alias so the sourceCTEName alias can be found as a relationship
          then fromMaybe (qiName parentNodeQi) parentAlias
          else qiName parentNodeQi
        rel = findRel schema allRels origin relName relHint
      in
      Node <$> newReadPlan <*> (updateForest . hush $ Node <$> newReadPlan <*> pure forest)
    Nothing -> -- root case
      let
        newFrom  = QualifiedIdentifier mempty $ decodeUtf8 sourceCTEName
        newAlias = Just (qiName $ from rPlan)
        newReadPlan = case action of
          -- the CTE for mutations/rpc is used as WITH sourceCTEName .. SELECT .. FROM sourceCTEName as alias,
          -- we use the table name as an alias so findRel can find the right relationship.
          ActionMutate _ -> rPlan{from=newFrom, fromAlias=newAlias}
          ActionInvoke _ -> rPlan{from=newFrom, fromAlias=newAlias}
          _              -> rPlan
      in
      Node newReadPlan <$> updateForest (Just $ Node newReadPlan forest)
  where
    updateForest :: Maybe ReadPlanTree -> Either ApiRequestError [ReadPlanTree]
    updateForest rq = addRels schema action allRels rq `traverse` forest

getJoinConditions :: Maybe Alias -> Maybe Alias -> Relationship -> [JoinCondition]
getJoinConditions _ _ ComputedRelationship{} = []
getJoinConditions tblAlias parentAlias Relationship{relTable=qi,relForeignTable=fQi,relCardinality=card} =
  case card of
    M2M (Junction QualifiedIdentifier{qiName=jtn} _ _ jcols1 jcols2) ->
      (toJoinCondition Nothing Nothing ftN jtn <$> jcols2) ++ (toJoinCondition parentAlias tblAlias tN jtn <$> jcols1)
    O2M _ cols ->
      toJoinCondition parentAlias tblAlias tN ftN <$> cols
    M2O _ cols ->
      toJoinCondition parentAlias tblAlias tN ftN <$> cols
    O2O _ cols ->
      toJoinCondition parentAlias tblAlias tN ftN <$> cols
  where
    QualifiedIdentifier{qiSchema=tSchema, qiName=tN} = qi
    QualifiedIdentifier{qiName=ftN} = fQi
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

-- Validates that the related resource on the order is an embedded resource,
-- e.g. if `clients` is inside the `select` in /projects?order=clients(id)&select=*,clients(*),
-- and if it's a to-one relationship, it adds the right alias to the OrderRelationTerm so the generated query can succeed.
-- TODO might be clearer if there's an additional intermediate type
addRelatedOrders :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
addRelatedOrders (Node rp@ReadPlan{order,from} forest) = do
  newOrder <- getRelOrder `traverse` order
  Node rp{order=newOrder} <$> addRelatedOrders `traverse` forest
  where
    getRelOrder ot@OrderTerm{}                   = Right ot
    getRelOrder ot@OrderRelationTerm{otRelation} =
      let foundRP = rootLabel <$> find (\(Node ReadPlan{relName, relAlias} _) -> otRelation == fromMaybe relName relAlias) forest in
      case foundRP of
        Just ReadPlan{relName,relAlias,relAggAlias,relToParent} ->
          let isToOne = relIsToOne <$> relToParent
              name    = fromMaybe relName relAlias in
          if isToOne == Just True
            then Right $ ot{otRelation=relAggAlias}
            else Left $ RelatedOrderNotToOne (qiName from) name
        Nothing ->
          Left $ NotEmbedded otRelation

-- Searches for null filters on embeds, e.g. `clients` on /projects?select=*,clients()&clients=not.is.null.
-- If these are found, it changes the filter to use the internal aggregate name(`projects_clients_1`) so the filter can succeed.
-- It fails if operators other than is.null or not.is.null are used.
addNullEmbedFilters :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
addNullEmbedFilters (Node rp@ReadPlan{where_=oldLogic} forest) = do
  let readPlans = rootLabel <$> forest
  newLogic <- getFilters readPlans `traverse` oldLogic
  Node rp{ReadPlan.where_= newLogic} <$> (addNullEmbedFilters `traverse` forest)
  where
    getFilters :: [ReadPlan] -> LogicTree -> Either ApiRequestError LogicTree
    getFilters rPlans (Expr b lOp trees) = Expr b lOp <$> (getFilters rPlans `traverse` trees)
    getFilters rPlans flt@(Stmnt (Filter (fld, []) opExpr)) =
      let foundRP = find (\ReadPlan{relName, relAlias} -> fld == fromMaybe relName relAlias) rPlans in
      case (foundRP, opExpr) of
        (Just ReadPlan{relAggAlias}, OpExpr b (Is TriNull)) -> Right $ Stmnt $ FilterNullEmbed b relAggAlias
        (Just ReadPlan{relName}, _)                         -> Left $ UnacceptableFilter relName
        _                                                   -> Right flt
    getFilters _ flt@(Stmnt _)        = Right flt

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

-- Validates that spread embeds are only done on to-one relationships
validateSpreadEmbeds :: ReadPlanTree -> Either ApiRequestError ReadPlanTree
validateSpreadEmbeds (Node rp@ReadPlan{relToParent=Nothing} forest) = Node rp <$> validateSpreadEmbeds `traverse` forest
validateSpreadEmbeds (Node rp@ReadPlan{relIsSpread,relToParent=Just rel,relName} forest) = do
  validRP <- if relIsSpread && not (relIsToOne rel)
    then Left $ SpreadNotToOne (qiName $ relTable rel) relName -- TODO using relTable is not entirely right because ReadPlan might have an alias, need to store the parent alias on ReadPlan
    else Right rp
  Node validRP <$> validateSpreadEmbeds `traverse` forest

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
    findNode = find (\(Node ReadPlan{relName, relAlias} _) -> relName == targetNodeName || relAlias == Just targetNodeName) forest

mutatePlan :: Mutation -> QualifiedIdentifier -> ApiRequest -> SchemaCache -> ReadPlanTree -> Either Error MutatePlan
mutatePlan mutation qi ApiRequest{..} sCache readReq = mapLeft ApiRequestError $
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
        else inferColsEmbedNeeds readReq pkCols
    pkCols = maybe mempty tablePKCols $ HM.lookup qi $ dbTables sCache
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
, funCReturning = inferColsEmbedNeeds readReq []
}
  where
    paramsAsSingleObject = iPreferParameters apiReq == Just SingleObject
    callParams = case pdParams proc of
      [prm] | paramsAsSingleObject -> OnePosParam prm
            | ppName prm == mempty -> OnePosParam prm
            | otherwise            -> KeyParams $ specifiedParams [prm]
      prms  -> KeyParams $ specifiedParams prms
    specifiedParams = filter (\x -> ppName x `S.member` iColumns apiReq)

-- | Infers the columns needed for an embed to be successful after a mutation or a function call.
inferColsEmbedNeeds :: ReadPlanTree -> [FieldName] -> [FieldName]
inferColsEmbedNeeds (Node ReadPlan{select} forest) pkCols
  -- if * is part of the select, we must not add pk or fk columns manually -
  -- otherwise those would be selected and output twice
  | "*" `elem` fldNames = ["*"]
  | otherwise           = returnings
  where
    fldNames = (\((fld, _), _, _) -> fld) <$> select
    -- Without fkCols, when a mutatePlan to
    -- /projects?select=name,clients(name) occurs, the RETURNING SQL part would
    -- be `RETURNING name`(see QueryBuilder).  This would make the embedding
    -- fail because the following JOIN would need the "client_id" column from
    -- projects.  So this adds the foreign key columns to ensure the embedding
    -- succeeds, result would be `RETURNING name, client_id`.
    fkCols = concat $ mapMaybe (\case
        Node ReadPlan{relToParent=Just Relationship{relCardinality=O2M _ cols}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{relToParent=Just Relationship{relCardinality=M2O _ cols}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{relToParent=Just Relationship{relCardinality=O2O _ cols}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{relToParent=Just Relationship{relCardinality=M2M Junction{junColsSource=cols}}} _ ->
          Just $ fst <$> cols
        Node ReadPlan{relToParent=Just ComputedRelationship{}} _ ->
          Nothing
        Node ReadPlan{relToParent=Nothing} _ ->
          Nothing
      ) forest
    hasComputedRel = isJust $ find (\case
      Node ReadPlan{relToParent=Just ComputedRelationship{}} _ -> True
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
