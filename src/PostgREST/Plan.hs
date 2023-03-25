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
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE RecordWildCards       #-}

module PostgREST.Plan
  ( wrappedReadPlan
  , mutateReadPlan
  , callReadPlan
  , WrappedReadPlan(..)
  , MutateReadPlan(..)
  , CallReadPlan(..)
  , inspectPlanTxMode
  ) where


import qualified Data.ByteString.Lazy       as LBS
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as L
import qualified Data.Set                   as S
import qualified PostgREST.SchemaCache.Proc as Proc

import Data.Either.Combinators (mapLeft, mapRight)
import Data.List               (delete)
import Data.Tree               (Tree (..))

import PostgREST.ApiRequest               (Action (..),
                                           ApiRequest (..),
                                           InvokeMethod (..),
                                           Mutation (..),
                                           Payload (..))
import PostgREST.Config                   (AppConfig (..))
import PostgREST.Error                    (Error (..))
import PostgREST.MediaType                (MTPlanAttrs (..),
                                           MediaType (..))
import PostgREST.Query.SqlFragment        (sourceCTEName)
import PostgREST.RangeQuery               (NonnegRange, allRange,
                                           convertToLimitZeroRange,
                                           restrictRange)
import PostgREST.SchemaCache              (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers  (FieldName,
                                           QualifiedIdentifier (..),
                                           Schema)
import PostgREST.SchemaCache.Proc         (ProcDescription (..),
                                           ProcParam (..), ProcsMap,
                                           procReturnsScalar)
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           RelationshipsMap,
                                           relIsToOne)
import PostgREST.SchemaCache.Table        (Table (tableName),
                                           tablePKCols)

import PostgREST.ApiRequest.Preferences
import PostgREST.ApiRequest.Types
import PostgREST.Plan.CallPlan
import PostgREST.Plan.MutatePlan
import PostgREST.Plan.ReadPlan          as ReadPlan
import PostgREST.Plan.Types

import qualified Hasql.Transaction.Sessions       as SQL
import qualified PostgREST.ApiRequest.QueryParams as QueryParams

import Protolude hiding (from)

data WrappedReadPlan = WrappedReadPlan {
  wrReadPlan :: ReadPlanTree
, wrTxMode   :: SQL.Mode
, wrBinField :: Maybe FieldName
}

data MutateReadPlan = MutateReadPlan {
  mrReadPlan   :: ReadPlanTree
, mrMutatePlan :: MutatePlan
, mrTxMode     :: SQL.Mode
}

data CallReadPlan = CallReadPlan {
  crReadPlan :: ReadPlanTree
, crCallPlan :: CallPlan
, crTxMode   :: SQL.Mode
, crProc     :: ProcDescription
, crBinField :: Maybe FieldName
}

wrappedReadPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> Either Error WrappedReadPlan
wrappedReadPlan  identifier conf sCache apiRequest = do
  rPlan <- readPlan identifier conf sCache apiRequest
  binField <- mapLeft ApiRequestError $ binaryField conf (iAcceptMediaType apiRequest) Nothing rPlan
  return $ WrappedReadPlan rPlan SQL.Read binField

mutateReadPlan :: Mutation -> ApiRequest -> QualifiedIdentifier -> AppConfig -> SchemaCache -> Either Error MutateReadPlan
mutateReadPlan  mutation apiRequest identifier conf sCache = do
  rPlan <- readPlan identifier conf sCache apiRequest
  mPlan <- mutatePlan mutation identifier apiRequest sCache rPlan
  return $ MutateReadPlan rPlan mPlan SQL.Write

callReadPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> InvokeMethod -> Either Error CallReadPlan
callReadPlan identifier conf sCache apiRequest invMethod = do
  let paramKeys = case invMethod of
        InvGet  -> S.fromList $ fst <$> qsParams'
        InvHead -> S.fromList $ fst <$> qsParams'
        InvPost -> iColumns apiRequest
  proc@ProcDescription{..} <- mapLeft ApiRequestError $
    findProc identifier paramKeys (preferParameters == Just SingleObject) (dbProcs sCache) (iContentMediaType apiRequest) (invMethod == InvPost)
  let relIdentifier = QualifiedIdentifier pdSchema (fromMaybe pdName $ Proc.procTableName proc) -- done so a set returning function can embed other relations
  rPlan <- readPlan relIdentifier conf sCache apiRequest
  let args = case (invMethod, iContentMediaType apiRequest) of
        (InvGet, _)             -> jsonRpcParams proc qsParams'
        (InvHead, _)            -> jsonRpcParams proc qsParams'
        (InvPost, MTUrlEncoded) -> maybe mempty (jsonRpcParams proc . payArray) $ iPayload apiRequest
        (InvPost, _)            -> maybe mempty payRaw $ iPayload apiRequest
      txMode = case (invMethod, pdVolatility) of
          (InvGet,  _)              -> SQL.Read
          (InvHead, _)              -> SQL.Read
          (InvPost, Proc.Stable)    -> SQL.Read
          (InvPost, Proc.Immutable) -> SQL.Read
          (InvPost, Proc.Volatile)  -> SQL.Write
      cPlan = callPlan proc apiRequest paramKeys args rPlan
  binField <- mapLeft ApiRequestError $ binaryField conf (iAcceptMediaType apiRequest) (Just proc) rPlan
  return $ CallReadPlan rPlan cPlan txMode proc binField
  where
    Preferences{..} = iPreferences apiRequest
    qsParams' = QueryParams.qsParams (iQueryParams apiRequest)

{-|
  Search a pg proc by matching name and arguments keys to parameters. Since a function can be overloaded,
  the name is not enough to find it. An overloaded function can have a different volatility or even a different return type.
-}
findProc :: QualifiedIdentifier -> S.Set Text -> Bool -> ProcsMap -> MediaType -> Bool -> Either ApiRequestError ProcDescription
findProc qi argumentsKeys paramsAsSingleObject allProcs contentMediaType isInvPost =
  case matchProc of
    ([], [])     -> Left $ NoRpc (qiSchema qi) (qiName qi) (S.toList argumentsKeys) paramsAsSingleObject contentMediaType isInvPost (HM.keys allProcs) lookupProcName
    -- If there are no functions with named arguments, fallback to the single unnamed argument function
    ([], [proc]) -> Right proc
    ([], procs)  -> Left $ AmbiguousRpc (toList procs)
    -- Matches the functions with named arguments
    ([proc], _)  -> Right proc
    (procs, _)   -> Left $ AmbiguousRpc (toList procs)
  where
    matchProc = overloadedProcPartition lookupProcName
    -- First find the proc by name
    lookupProcName = HM.lookupDefault mempty qi allProcs
    -- The partition obtained has the form (overloadedProcs,fallbackProcs)
    -- where fallbackProcs are functions with a single unnamed parameter
    overloadedProcPartition = foldr select ([],[])
    select proc ~(ts,fs)
      | matchesParams proc         = (proc:ts,fs)
      | hasSingleUnnamedParam proc = (ts,proc:fs)
      | otherwise                  = (ts,fs)
    -- If the function is called with post and has a single unnamed parameter
    -- it can be called depending on content type and the parameter type
    hasSingleUnnamedParam ProcDescription{pdParams=[ProcParam{ppType}]} = isInvPost && case (contentMediaType, ppType) of
      (MTApplicationJSON, "json")  -> True
      (MTApplicationJSON, "jsonb") -> True
      (MTTextPlain, "text")        -> True
      (MTTextXML, "xml")           -> True
      (MTOctetStream, "bytea")     -> True
      _                            -> False
    hasSingleUnnamedParam _ = False
    matchesParams proc =
      let
        params = pdParams proc
        firstType = (ppType <$> headMay params)
      in
      -- exceptional case for Prefer: params=single-object
      if paramsAsSingleObject
        then length params == 1 && (firstType == Just "json" || firstType == Just "jsonb")
      -- If the function has no parameters, the arguments keys must be empty as well
      else if null params
        then null argumentsKeys && not (isInvPost && contentMediaType `elem` [MTOctetStream, MTTextPlain, MTTextXML])
      -- A function has optional and required parameters. Optional parameters have a default value and
      -- don't require arguments for the function to be executed, required parameters must have an argument present.
      else case L.partition ppReq params of
      -- If the function only has required parameters, the arguments keys must match those parameters
        (reqParams, [])        -> argumentsKeys == S.fromList (ppName <$> reqParams)
      -- If the function only has optional parameters, the arguments keys can match none or any of them(a subset)
        ([], optParams)        -> argumentsKeys `S.isSubsetOf` S.fromList (ppName <$> optParams)
      -- If the function has required and optional parameters, the arguments keys have to match the required parameters
      -- and can match any or none of the default parameters.
        (reqParams, optParams) -> argumentsKeys `S.difference` S.fromList (ppName <$> optParams) == S.fromList (ppName <$> reqParams)

inspectPlanTxMode :: SQL.Mode
inspectPlanTxMode = SQL.Read

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
    []  -> Left $ NoRelBetween origin target hint schema allRels
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
        ActionInvoke _ -> qsFilters
        ActionRead _   -> qsFilters
        _              -> qsFiltersNotRoot

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
mutatePlan mutation qi ApiRequest{iPreferences=preferences, ..} sCache readReq = mapLeft ApiRequestError $
  case mutation of
    MutationCreate ->
      mapRight (\typedColumns -> Insert qi typedColumns body ((,) <$> preferences.preferResolution <*> Just confCols) [] returnings pkCols applyDefaults) typedColumnsOrError
    MutationUpdate ->
      mapRight (\typedColumns -> Update qi typedColumns body combinedLogic iTopLevelRange rootOrder returnings applyDefaults applyIgnoreValues pkCols isBulkUpdate) typedColumnsOrError
    MutationSingleUpsert ->
        if null qsLogic &&
           qsFilterFields == S.fromList pkCols &&
           not (null (S.fromList pkCols)) &&
           all (\case
              Filter _ (OpExpr False (Op OpEqual _)) -> True
              _                                      -> False) qsFiltersRoot
          then mapRight (\typedColumns -> Insert qi typedColumns body (Just (MergeDuplicates, pkCols)) combinedLogic returnings mempty False) typedColumnsOrError
        else
          Left InvalidFilters
    MutationDelete -> Right $ Delete qi combinedLogic iTopLevelRange rootOrder returnings
  where
    confCols = fromMaybe pkCols qsOnConflict
    QueryParams.QueryParams{..} = iQueryParams
    returnings =
      if preferences.preferRepresentation == None
        then []
        else inferColsEmbedNeeds readReq pkCols
    pkCols = maybe mempty tablePKCols $ HM.lookup qi $ dbTables sCache
    logic = map snd qsLogic
    rootOrder = maybe [] snd $ find (\(x, _) -> null x) qsOrder
    combinedLogic = foldr addFilterToLogicForest logic qsFiltersRoot
    body = payRaw <$> iPayload -- the body is assumed to be json at this stage(ApiRequest validates)
    tbl = HM.lookup qi $ dbTables sCache
    typedColumnsOrError = resolveOrError tbl `traverse` S.toList iColumns
    applyDefaults = preferences.preferUndefinedKeys == Just ApplyDefaults
    applyIgnoreValues = preferences.preferUndefinedKeys == Just IgnoreValues
    isBulkUpdate = preferences.preferParameters == Just MultipleObjects

resolveOrError :: Maybe Table -> FieldName -> Either ApiRequestError TypedField
resolveOrError Nothing _ = Left NotFound
resolveOrError (Just table) field =
  case resolveTableField table field of
    Nothing         -> Left $ ColumnNotFound (tableName table) field
    Just typedField -> Right typedField

callPlan :: ProcDescription -> ApiRequest -> S.Set FieldName -> LBS.ByteString -> ReadPlanTree -> CallPlan
callPlan proc ApiRequest{iPreferences=Preferences{..}} paramKeys args readReq = FunctionCall {
  funCQi = QualifiedIdentifier (pdSchema proc) (pdName proc)
, funCParams = callParams
, funCArgs = Just args
, funCScalar = procReturnsScalar proc
, funCMultipleCall = preferParameters == Just MultipleObjects
, funCReturning = inferColsEmbedNeeds readReq []
}
  where
    paramsAsSingleObject = preferParameters == Just SingleObject
    specifiedParams = filter (\x -> ppName x `S.member` paramKeys)
    callParams = case pdParams proc of
      [prm] | paramsAsSingleObject -> OnePosParam prm
            | ppName prm == mempty -> OnePosParam prm
            | otherwise            -> KeyParams $ specifiedParams [prm]
      prms  -> KeyParams $ specifiedParams prms

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

-- | If raw(binary) output is requested, check that MediaType is one of the
-- admitted rawMediaTypes and that`?select=...` contains only one field other
-- than `*`
binaryField :: AppConfig -> MediaType -> Maybe ProcDescription -> ReadPlanTree -> Either ApiRequestError (Maybe FieldName)
binaryField AppConfig{configRawMediaTypes} acceptMediaType proc rpTree
  | isRawMediaType =
    if (procReturnsScalar <$> proc) == Just True
      then Right $ Just "pgrst_scalar"
      else
        let
          fieldName = fstFieldName rpTree
        in
        case fieldName of
          Just fld -> Right $ Just fld
          Nothing  -> Left $ BinaryFieldError acceptMediaType
  | otherwise =
      Right Nothing
  where
    isRawMediaType = acceptMediaType `elem` configRawMediaTypes `L.union` [MTOctetStream, MTTextPlain, MTTextXML] || isRawPlan acceptMediaType
    isRawPlan mt = case mt of
      MTPlan (MTPlanAttrs (Just MTOctetStream) _ _) -> True
      MTPlan (MTPlanAttrs (Just MTTextPlain) _ _)   -> True
      MTPlan (MTPlanAttrs (Just MTTextXML) _ _)     -> True
      _                                             -> False

    fstFieldName :: ReadPlanTree -> Maybe FieldName
    fstFieldName (Node ReadPlan{select=(("*", []), _, _):_} [])  = Nothing
    fstFieldName (Node ReadPlan{select=[((fld, []), _, _)]} []) = Just fld
    fstFieldName _                                               = Nothing
