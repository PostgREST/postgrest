{-|
Module      : PostgREST.DbRequestBuilder
Description : PostgREST database request builder

This module is in charge of building an intermediate representation(ReadRequest, MutateRequest) between the HTTP request and the final resulting SQL query.

A query tree is built in case of resource embedding. By inferring the relationship between tables, join conditions are added for every embedded resource.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module PostgREST.DbRequestBuilder
  ( readRequest
  , mutateRequest
  , returningCols
  ) where

import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

import Control.Arrow           ((***))
import Data.Either.Combinators (mapLeft)
import Data.List               (delete)
import Data.Text               (isInfixOf)

import Control.Applicative
import Data.Tree

import PostgREST.ApiRequest       (Action (..), ApiRequest (..))
import PostgREST.DbStructureTypes
import PostgREST.Error            (ApiRequestError (..), Error (..))
import PostgREST.Parsers
import PostgREST.Preferences
import PostgREST.Private.QueryFragment (sourceCTEName)
import PostgREST.RangeQuery       (NonnegRange, allRange,
                                   restrictRange)
import PostgREST.Types
import Protolude                  hiding (from)

-- | Builds the ReadRequest tree on a number of stages.
-- | Adds filters, order, limits on its respective nodes.
-- | Adds joins conditions obtained from resource embedding.
readRequest :: Schema -> TableName -> Maybe Integer -> [Relation] -> ApiRequest -> Either Error ReadRequest
readRequest schema rootTableName maxRows allRels apiRequest  =
  mapLeft ApiRequestError $
  treeRestrictRange maxRows =<<
  augmentRequestWithJoin schema rootRels =<<
  addFiltersOrdersRanges apiRequest =<<
  (initReadRequest rootName <$> pRequestSelect sel)
  where
    sel = fromMaybe "*" $ iSelect apiRequest -- default to all columns requested (SELECT *) for a non existent ?select querystring param
    (rootName, rootRels) = rootWithRels schema rootTableName allRels (iAction apiRequest)

-- Get the root table name with its relationships according to the Action type.
-- This is done because of the shape of the final SQL Query. The mutation cases are wrapped in a WITH {sourceCTEName}(see Statements.hs).
-- So we need a FROM {sourceCTEName} instead of FROM {tableName}.
rootWithRels :: Schema -> TableName -> [Relation] -> Action -> (QualifiedIdentifier, [Relation])
rootWithRels schema rootTableName allRels action = case action of
  ActionRead _ -> (QualifiedIdentifier schema rootTableName, allRels) -- normal read case
  _            -> (QualifiedIdentifier mempty _sourceCTEName, mapMaybe toSourceRel allRels ++ allRels) -- mutation cases and calling proc
  where
    _sourceCTEName = decodeUtf8 sourceCTEName
    -- To enable embedding in the sourceCTEName cases we need to replace the foreign key tableName in the Relation
    -- with {sourceCTEName}. This way findRel can find relationships with sourceCTEName.
    toSourceRel :: Relation -> Maybe Relation
    toSourceRel r@Relation{relTable=t}
      | rootTableName == tableName t = Just $ r {relTable=t {tableName=_sourceCTEName}}
      | otherwise                    = Nothing

-- Build the initial tree with a Depth attribute so when a self join occurs we can differentiate the parent and child tables by having
-- an alias like "table_depth", this is related to http://github.com/PostgREST/postgrest/issues/987.
initReadRequest :: QualifiedIdentifier -> [Tree SelectItem] -> ReadRequest
initReadRequest rootQi =
  foldr (treeEntry rootDepth) initial
  where
    rootDepth = 0
    rootSchema = qiSchema rootQi
    rootName = qiName rootQi
    initial = Node (Select [] rootQi Nothing [] [] [] [] allRange, (rootName, Nothing, Nothing, Nothing, rootDepth)) []
    treeEntry :: Depth -> Tree SelectItem -> ReadRequest -> ReadRequest
    treeEntry depth (Node fld@((fn, _),_,alias, embedHint) fldForest) (Node (q, i) rForest) =
      let nxtDepth = succ depth in
      case fldForest of
        [] -> Node (q {select=fld:select q}, i) rForest
        _  -> Node (q, i) $
              foldr (treeEntry nxtDepth)
              (Node (Select [] (QualifiedIdentifier rootSchema fn) Nothing [] [] [] [] allRange,
                (fn, Nothing, alias, embedHint, nxtDepth)) [])
              fldForest:rForest

-- | Enforces the `max-rows` config on the result
treeRestrictRange :: Maybe Integer -> ReadRequest -> Either ApiRequestError ReadRequest
treeRestrictRange maxRows request = pure $ nodeRestrictRange maxRows <$> request
  where
    nodeRestrictRange :: Maybe Integer -> ReadNode -> ReadNode
    nodeRestrictRange m (q@Select {range_=r}, i) = (q{range_=restrictRange m r }, i)

augmentRequestWithJoin :: Schema -> [Relation] -> ReadRequest -> Either ApiRequestError ReadRequest
augmentRequestWithJoin schema allRels request =
  addRels schema allRels Nothing request
  >>= addJoinConditions Nothing

addRels :: Schema -> [Relation] -> Maybe ReadRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addRels schema allRels parentNode (Node (query@Select{from=tbl}, (nodeName, _, alias, hint, depth)) forest) =
  case parentNode of
    Just (Node (Select{from=parentNodeQi}, _) _) ->
      let newFrom r = if qiName tbl == nodeName then tableQi (relFTable r) else tbl
          newReadNode = (\r -> (query{from=newFrom r}, (nodeName, Just r, alias, Nothing, depth))) <$> rel
          rel = findRel schema allRels (qiName parentNodeQi) nodeName hint
      in
      Node <$> newReadNode <*> (updateForest . hush $ Node <$> newReadNode <*> pure forest)
    _ ->
      let rn = (query, (nodeName, Nothing, alias, Nothing, depth)) in
      Node rn <$> updateForest (Just $ Node rn forest)
  where
    updateForest :: Maybe ReadRequest -> Either ApiRequestError [ReadRequest]
    updateForest rq = addRels schema allRels rq `traverse` forest

-- Finds a relationship between an origin and a target in the request: /origin?select=target(*)
-- If more than one relationship is found then the request is ambiguous and we return an error.
-- In that case the request can be disambiguated by adding precision to the target or by using a hint: /origin?select=target!hint(*)
-- The elements will be matched according to these rules:
-- origin = table / view
-- target = table / view / constraint / column-from-origin
-- hint   = table / view / constraint / column-from-origin / column-from-target
-- (hint can take table / view values to aid in finding the junction in an m2m relationship)
findRel :: Schema -> [Relation] -> NodeName -> NodeName -> Maybe EmbedHint -> Either ApiRequestError Relation
findRel schema allRels origin target hint =
  case rel of
    []  -> Left $ NoRelBetween origin target
    [r] -> Right r
    rs  ->
      -- Return error if more than one relationship is found, unless we're in a self reference case.
      --
      -- Here we handle a self reference relationship to not cause a breaking change:
      -- In a self reference we get two relationships with the same foreign key and relTable/relFtable but with different cardinalities(m2o/o2m)
      -- We output the O2M rel, the M2O rel can be obtained by using the origin column as an embed hint.
      let [rel0, rel1] = take 2 rs in
      if length rs == 2 && relLink rel0 == relLink rel1 && relTable rel0 == relTable rel1 && relFTable rel0 == relFTable rel1
        then note (NoRelBetween origin target) (find (\r -> relType r == O2M) rs)
        else Left $ AmbiguousRelBetween origin target rs
  where
    matchFKSingleCol hint_ cols = length cols == 1 && hint_ == (colName <$> head cols)
    rel = filter (
      \Relation{..} ->
        -- Both relationship ends need to be on the exposed schema
        schema == tableSchema relTable && schema == tableSchema relFTable &&
        (
          -- /projects?select=clients(*)
          origin == tableName relTable  &&  -- projects
          target == tableName relFTable ||  -- clients

          -- /projects?select=projects_client_id_fkey(*)
          (
            origin == tableName relTable && -- projects
            Constraint target == relLink    -- projects_client_id_fkey
          ) ||
          -- /projects?select=client_id(*)
          (
            origin == tableName relTable &&           -- projects
            matchFKSingleCol (Just target) relColumns -- client_id
          )
        ) && (
          isNothing hint || -- hint is optional

          -- /projects?select=clients!projects_client_id_fkey(*)
          (
            relType /= M2M &&
            hint == Just (constName relLink) -- projects_client_id_fkey
          ) ||

          -- /projects?select=clients!client_id(*) or /projects?select=clients!id(*)
          matchFKSingleCol hint relColumns  || -- client_id
          matchFKSingleCol hint relFColumns || -- id

          -- /users?select=tasks!users_tasks(*)
          (
            relType == M2M &&                           -- many-to-many between users and tasks
            hint == Just (tableName $ junTable relLink) -- users_tasks
          )
        )
      ) allRels

-- previousAlias is only used for the case of self joins
addJoinConditions :: Maybe Alias -> ReadRequest -> Either ApiRequestError ReadRequest
addJoinConditions previousAlias (Node node@(query@Select{from=tbl}, nodeProps@(_, rel, _, _, depth)) forest) =
  case rel of
    Just r@Relation{relType=M2M, relLink=Junction{junTable}} ->
      let rq = augmentQuery r in
      Node (rq{implicitJoins=tableQi junTable:implicitJoins rq}, nodeProps) <$> updatedForest
    Just r -> Node (augmentQuery r, nodeProps) <$> updatedForest
    Nothing -> Node node <$> updatedForest
  where
    newAlias = case isSelfReference <$> rel of
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
getJoinConditions :: Maybe Alias -> Maybe Alias -> Relation -> [JoinCondition]
getJoinConditions previousAlias newAlias (Relation Table{tableSchema=tSchema, tableName=tN} cols Table{tableName=ftN} fCols _ lnk) =
  case lnk of
    Junction Table{tableName=jtn} _ jc1 _ jc2 ->
      zipWith (toJoinCondition tN jtn) cols jc1 ++ zipWith (toJoinCondition ftN jtn) fCols jc2
    Constraint _ ->
      zipWith (toJoinCondition tN ftN) cols fCols
  where
    toJoinCondition :: Text -> Text -> Column -> Column -> JoinCondition
    toJoinCondition tb ftb c fc =
      let qi1 = removeSourceCTESchema tSchema tb
          qi2 = removeSourceCTESchema tSchema ftb in
        JoinCondition (maybe qi1 (QualifiedIdentifier mempty) previousAlias, colName c)
                      (maybe qi2 (QualifiedIdentifier mempty) newAlias, colName fc)

    -- On mutation and calling proc cases we wrap the target table in a WITH {sourceCTEName}
    -- if this happens remove the schema `FROM "schema"."{sourceCTEName}"` and use only the
    -- `FROM "{sourceCTEName}"`. If the schema remains the FROM would be invalid.
    removeSourceCTESchema :: Schema -> TableName -> QualifiedIdentifier
    removeSourceCTESchema schema tbl = QualifiedIdentifier (if tbl == decodeUtf8 sourceCTEName then mempty else schema) tbl

addFiltersOrdersRanges :: ApiRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addFiltersOrdersRanges apiRequest rReq = do
  rFlts <- foldr addFilter rReq <$> filters
  rOrds <- foldr addOrder rFlts <$> orders
  rRngs <- foldr addRange rOrds <$> ranges
  foldr addLogicTree rRngs      <$> logicForest
  where
    filters :: Either ApiRequestError [(EmbedPath, Filter)]
    filters = pRequestFilter `traverse` flts
    orders :: Either ApiRequestError [(EmbedPath, [OrderTerm])]
    orders = pRequestOrder `traverse` iOrder apiRequest
    ranges :: Either ApiRequestError [(EmbedPath, NonnegRange)]
    ranges = pRequestRange `traverse` M.toList (iRange apiRequest)
    logicForest :: Either ApiRequestError [(EmbedPath, LogicTree)]
    logicForest = pRequestLogicTree `traverse` logFrst
    action = iAction apiRequest
    -- there can be no filters on the root table when we are doing insert/update/delete
    (flts, logFrst) =
      case action of
        ActionInvoke _ -> (iFilters apiRequest, iLogic apiRequest)
        ActionRead _   -> (iFilters apiRequest, iLogic apiRequest)
        _              -> join (***) (filter (( "." `isInfixOf` ) . fst)) (iFilters apiRequest, iLogic apiRequest)

addFilterToNode :: Filter -> ReadRequest -> ReadRequest
addFilterToNode flt (Node (q@Select {where_=lf}, i) f) = Node (q{where_=addFilterToLogicForest flt lf}::ReadQuery, i) f

addFilter :: (EmbedPath, Filter) -> ReadRequest -> ReadRequest
addFilter = addProperty addFilterToNode

addOrderToNode :: [OrderTerm] -> ReadRequest -> ReadRequest
addOrderToNode o (Node (q,i) f) = Node (q{order=o}, i) f

addOrder :: (EmbedPath, [OrderTerm]) -> ReadRequest -> ReadRequest
addOrder = addProperty addOrderToNode

addRangeToNode :: NonnegRange -> ReadRequest -> ReadRequest
addRangeToNode r (Node (q,i) f) = Node (q{range_=r}, i) f

addRange :: (EmbedPath, NonnegRange) -> ReadRequest -> ReadRequest
addRange = addProperty addRangeToNode

addLogicTreeToNode :: LogicTree -> ReadRequest -> ReadRequest
addLogicTreeToNode t (Node (q@Select{where_=lf},i) f) = Node (q{where_=t:lf}::ReadQuery, i) f

addLogicTree :: (EmbedPath, LogicTree) -> ReadRequest -> ReadRequest
addLogicTree = addProperty addLogicTreeToNode

addProperty :: (a -> ReadRequest -> ReadRequest) -> (EmbedPath, a) -> ReadRequest -> ReadRequest
addProperty f ([], a) rr = f a rr
addProperty f (targetNodeName:remainingPath, a) (Node rn forest) =
  case pathNode of
    Nothing -> Node rn forest -- the property is silenty dropped in the Request does not contain the required path
    Just tn -> Node rn (addProperty f (remainingPath, a) tn:delete tn forest)
  where
    pathNode = find (\(Node (_,(nodeName,_,alias,_,_)) _) -> nodeName == targetNodeName || alias == Just targetNodeName) forest

mutateRequest :: Schema -> TableName -> ApiRequest -> [FieldName] -> ReadRequest -> Either Error MutateRequest
mutateRequest schema tName apiRequest pkCols readReq = mapLeft ApiRequestError $
  case action of
    ActionCreate -> do
        confCols <- case iOnConflict apiRequest of
            Nothing    -> pure pkCols
            Just param -> pRequestOnConflict param
        pure $ Insert qi (iColumns apiRequest) body ((,) <$> iPreferResolution apiRequest <*> Just confCols) [] returnings
    ActionUpdate -> Update qi (iColumns apiRequest) body  <$> combinedLogic <*> pure returnings
    ActionSingleUpsert ->
      (\flts ->
        if null (iLogic apiRequest) &&
           S.fromList (fst <$> iFilters apiRequest) == S.fromList pkCols &&
           not (null (S.fromList pkCols)) &&
           all (\case
              Filter _ (OpExpr False (Op "eq" _)) -> True
              _                                   -> False) flts
          then Insert qi (iColumns apiRequest) body (Just (MergeDuplicates, pkCols)) <$> combinedLogic <*> pure returnings
        else
          Left InvalidFilters) =<< filters
    ActionDelete -> Delete qi <$> combinedLogic <*> pure returnings
    _            -> Left UnsupportedVerb
  where
    qi = QualifiedIdentifier schema tName
    action = iAction apiRequest
    returnings =
      if iPreferRepresentation apiRequest == None
        then []
        else returningCols readReq pkCols
    filters = map snd <$> pRequestFilter `traverse` mutateFilters
    logic = map snd <$> pRequestLogicTree `traverse` logicFilters
    combinedLogic = foldr addFilterToLogicForest <$> logic <*> filters
    -- update/delete filters can be only on the root table
    (mutateFilters, logicFilters) = join (***) onlyRoot (iFilters apiRequest, iLogic apiRequest)
    onlyRoot = filter (not . ( "." `isInfixOf` ) . fst)
    body = pjRaw <$> iPayload apiRequest

returningCols :: ReadRequest -> [FieldName] -> [FieldName]
returningCols rr@(Node _ forest) pkCols
  -- if * is part of the select, we must not add pk or fk columns manually - otherwise those would be selected and output twice
  | "*" `elem` fldNames = ["*"]
  | otherwise           = returnings
  where
    fldNames = fstFieldNames rr
    -- Without fkCols, when a mutateRequest to /projects?select=name,clients(name) occurs, the RETURNING SQL part would be
    -- `RETURNING name`(see QueryBuilder).
    -- This would make the embedding fail because the following JOIN would need the "client_id" column from projects.
    -- So this adds the foreign key columns to ensure the embedding succeeds, result would be `RETURNING name, client_id`.
    -- This also works for the other relType's.
    fkCols = concat $ mapMaybe (\case
        Node (_, (_, Just Relation{relColumns=cols, relType=relTyp}, _, _, _)) _ -> case relTyp of
          O2M -> Just cols
          M2O -> Just cols
          M2M -> Just cols
        _ -> Nothing
      ) forest
    -- However if the "client_id" is present, e.g. mutateRequest to /projects?select=client_id,name,clients(name)
    -- we would get `RETURNING client_id, name, client_id` and then we would produce the "column reference \"client_id\" is ambiguous"
    -- error from PostgreSQL. So we deduplicate with Set:
    -- We are adding the primary key columns as well to make sure, that a proper location header can always be built for INSERT/POST
    returnings = S.toList . S.fromList $ fldNames ++ (colName <$> fkCols) ++ pkCols

-- Traditional filters(e.g. id=eq.1) are added as root nodes of the LogicTree
-- they are later concatenated with AND in the QueryBuilder
addFilterToLogicForest :: Filter -> [LogicTree] -> [LogicTree]
addFilterToLogicForest flt lf = Stmnt flt : lf
