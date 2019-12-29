{-|
Module      : PostgREST.DbRequestBuilder
Description : PostgREST database request builder

This module is in charge of building an intermediate representation(ReadRequest, MutateRequest) between the HTTP request and the final resulting SQL query.

A query tree is built in case of resource embedding. By inferring the relationship between tables, join conditions are added for every embedded resource.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

module PostgREST.DbRequestBuilder (
  readRequest
, mutateRequest
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S

import Control.Arrow           ((***))
import Data.Either.Combinators (mapLeft)
import Data.Foldable           (foldr1)
import Data.List               (delete, head, (!!))
import Data.Maybe              (fromJust)
import Data.Text               (isInfixOf)
import Text.Regex.TDFA         ((=~))
import Unsafe                  (unsafeHead)

import Control.Applicative
import Data.Tree
import Network.Wai

import PostgREST.ApiRequest (Action (..), ApiRequest (..))
import PostgREST.Error      (ApiRequestError (..), errorResponseFor)
import PostgREST.Parsers
import PostgREST.RangeQuery (NonnegRange, allRange, restrictRange)
import PostgREST.Types
import Protolude            hiding (from, head)

readRequest :: Schema -> TableName -> Maybe Integer -> [Relation] -> ApiRequest -> Either Response ReadRequest
readRequest schema rootTableName maxRows allRels apiRequest  =
  mapLeft errorResponseFor $
  treeRestrictRange maxRows =<<
  augumentRequestWithJoin schema rootRels =<<
  addFiltersOrdersRanges apiRequest <*>
  (initReadRequest rootName <$> pRequestSelect sel)
  where
    sel = fromMaybe "*" $ iSelect apiRequest -- default to all columns requested (SELECT *) for a non existent ?select querystring param
    (rootName, rootRels) = rootWithRelations schema rootTableName allRels (iAction apiRequest)

-- Get the root table name with its relations according to the Action type.
-- This is done because of the shape of the final SQL Query. The mutation cases are wrapped in a WITH {sourceCTEName}(see Statements.hs).
-- So we need a FROM {sourceCTEName} instead of FROM {tableName}.
rootWithRelations :: Schema -> TableName -> [Relation] -> Action -> (QualifiedIdentifier, [Relation])
rootWithRelations schema rootTableName allRels action = case action of
  ActionRead _ -> (QualifiedIdentifier schema rootTableName, allRels) -- normal read case
  _            -> (QualifiedIdentifier mempty sourceCTEName, mapMaybe toSourceRelation allRels ++ allRels) -- mutation cases and calling proc
  where
    -- To enable embedding in the sourceCTEName cases we need to replace the foreign key tableName in the Relation
    -- with {sourceCTEName}. This way findRelation can find Relations with sourceCTEName.
    toSourceRelation :: Relation -> Maybe Relation
    toSourceRelation r@Relation{relTable=t}
      | rootTableName == tableName t = Just $ r {relTable=t {tableName=sourceCTEName}}
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
    treeEntry depth (Node fld@((fn, _),_,alias,relationDetail) fldForest) (Node (q, i) rForest) =
      let nxtDepth = succ depth in
      case fldForest of
        [] -> Node (q {select=fld:select q}, i) rForest
        _  -> Node (q, i) $
              foldr (treeEntry nxtDepth)
              (Node (Select [] (QualifiedIdentifier rootSchema fn) Nothing [] [] [] [] allRange,
                (fn, Nothing, alias, relationDetail, nxtDepth)) [])
              fldForest:rForest

treeRestrictRange :: Maybe Integer -> ReadRequest -> Either ApiRequestError ReadRequest
treeRestrictRange maxRows request = pure $ nodeRestrictRange maxRows <$> request
  where
    nodeRestrictRange :: Maybe Integer -> ReadNode -> ReadNode
    nodeRestrictRange m (q@Select {range_=r}, i) = (q{range_=restrictRange m r }, i)

augumentRequestWithJoin :: Schema -> [Relation] -> ReadRequest -> Either ApiRequestError ReadRequest
augumentRequestWithJoin schema allRels request =
  addRelations schema allRels Nothing request
  >>= addJoinConditions Nothing

addRelations :: Schema -> [Relation] -> Maybe ReadRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addRelations schema allRelations parentNode (Node (query@Select{from=tbl}, (nodeName, _, alias, relationDetail, depth)) forest) =
  case parentNode of
    Just (Node (Select{from=parentNodeQi}, _) _) ->
      let newFrom r = if qiName tbl == nodeName then tableQi (relFTable r) else tbl
          newReadNode = (\r -> (query{from=newFrom r}, (nodeName, Just r, alias, Nothing, depth))) <$> rel
          parentNodeTable = qiName parentNodeQi
          results = findRelation schema allRelations parentNodeTable nodeName relationDetail
          rel :: Either ApiRequestError Relation
          rel = case results of
            []  -> Left $ NoRelBetween parentNodeTable nodeName
            [r] -> Right r
            rs  ->
              -- Hack for handling a self reference relationship.
              -- In this case we get an O2M and M2O rels with the same relTable and relFtable.
              -- We output the O2M rel, the M2O rel can be obtained by using the fk column as an embed hint in findRelation.
              let rel0 = head rs
                  rel1 = rs !! 1 in
              if length rs == 2 && relTable rel0 == relTable rel1 && relFTable rel0 == relFTable rel1
                then note (NoRelBetween parentNodeTable nodeName) (find (\r -> relType r == O2M) rs)
                else Left $ AmbiguousRelBetween parentNodeTable nodeName rs
      in
      Node <$> newReadNode <*> (updateForest . hush $ Node <$> newReadNode <*> pure forest)
    _ ->
      let rn = (query, (nodeName, Nothing, alias, Nothing, depth)) in
      Node rn <$> updateForest (Just $ Node rn forest)
  where
    updateForest :: Maybe ReadRequest -> Either ApiRequestError [ReadRequest]
    updateForest rq = mapM (addRelations schema allRelations rq) forest

findRelation :: Schema -> [Relation] -> TableName -> NodeName -> Maybe RelationDetail -> [Relation]
findRelation schema allRelations parentTableName nodeName relationDetail =
  filter (\Relation{relTable, relColumns, relFTable, relFColumns, relType, relLinkTable} ->
    -- Both relation ends need to be on the exposed schema
    schema == tableSchema relTable && schema == tableSchema relFTable &&
    case relationDetail of
      Nothing ->

        -- (request)        => projects { ..., clients{...} }
        -- will match
        -- (relation type)  => M2O
        -- (entity)         => clients  {id}
        -- (foriegn entity) => projects {client_id}
        (
          parentTableName == tableName relTable && -- projects
          nodeName == tableName relFTable -- clients
        ) ||

        -- (request)        => projects { ..., client_id{...} }
        -- will match
        -- (relation type)  => M2O
        -- (entity)         => clients  {id}
        -- (foriegn entity) => projects {client_id}
        (
          parentTableName == tableName relTable && -- projects
          length relColumns == 1 &&
          -- match common foreign key names(table_name_id, table_name_fk) to table_name
          (toS ("^" <> colName (unsafeHead relColumns) <> "_?(?:|[iI][dD]|[fF][kK])$") :: BS.ByteString) =~
          (toS nodeName :: BS.ByteString) -- client_id
        )

        -- (request)        => project_id { ..., client_id{...} }
        -- will match
        -- (relation type)  => M2O
        -- (entity)         => clients  {id}
        -- (foriegn entity) => projects {client_id}
        -- this case works becasue before reaching this place
        -- addRelation will turn project_id to project so the above condition will match

      Just rd ->

        -- (request)        => clients { ..., projects!client_id{...} }
        -- will match
        -- (relation type)  => O2M
        -- (entity)         => clients  {id}
        -- (foriegn entity) => projects {client_id}
        (
          relType == O2M &&
          parentTableName == tableName relTable && -- clients
          nodeName == tableName relFTable && -- projects
          length relFColumns == 1 &&
          rd == colName (unsafeHead relFColumns) -- rd is client_id
        ) ||

        -- (request)        => message { ..., person_detail!sender{...} }
        -- will match
        -- (relation type)  => M2O
        -- (entity)         => message  {sender}
        -- (foriegn entity) => person_detail {id}
        (
          relType == M2O &&
          parentTableName == tableName relTable && -- message
          nodeName == tableName relFTable && -- person_detail
          length relColumns == 1 &&
          rd == colName (unsafeHead relColumns) -- rd is sender
        ) ||

        -- (request)        => tasks { ..., users.tasks_users{...} }
        -- will match
        -- (relation type)  => M2M
        -- (entity)         => users
        -- (foriegn entity) => tasks
        (
          relType == M2M &&
          parentTableName == tableName relTable && -- tasks
          nodeName == tableName relFTable && -- users
          rd == tableName (fromJust relLinkTable) -- rd is tasks_users
        )
  ) allRelations

-- previousAlias is only used for the case of self joins
addJoinConditions :: Maybe Alias -> ReadRequest -> Either ApiRequestError ReadRequest
addJoinConditions previousAlias (Node node@(query@Select{from=tbl}, nodeProps@(_, relation, _, _, depth)) forest) =
  case relation of
    Just rel@Relation{relType=O2M} -> Node (augmentQuery rel, nodeProps) <$> updatedForest
    Just rel@Relation{relType=M2O} -> Node (augmentQuery rel, nodeProps) <$> updatedForest
    Just rel@Relation{relType=M2M, relLinkTable=lTable} ->
      case lTable of
        Just linkTable ->
          let rq = augmentQuery rel in
          Node (rq{implicitJoins=tableQi linkTable:implicitJoins rq}, nodeProps) <$> updatedForest
        Nothing ->
          Left UnknownRelation
    Nothing -> Node node <$> updatedForest
  where
    newAlias = case isSelfReference <$> relation of
      Just True
        | depth /= 0 -> Just (qiName tbl <> "_" <> show depth) -- root node doesn't get aliased
        | otherwise  -> Nothing
      _              -> Nothing
    augmentQuery rel =
      foldr
        (\jc rq@Select{joinConditions=jcs} -> rq{joinConditions=jc:jcs})
        query{fromAlias=newAlias}
        (getJoinConditions previousAlias newAlias rel)
    updatedForest = mapM (addJoinConditions newAlias) forest

-- previousAlias and newAlias are used in the case of self joins
getJoinConditions :: Maybe Alias -> Maybe Alias -> Relation -> [JoinCondition]
getJoinConditions previousAlias newAlias (Relation Table{tableSchema=tSchema, tableName=tN} cols Table{tableName=ftN} fCols typ lt lc1 lc2) =
  case typ of
    O2M ->
        zipWith (toJoinCondition tN ftN) cols fCols
    M2O ->
        zipWith (toJoinCondition tN ftN) cols fCols
    M2M ->
        let ltN = maybe "" tableName lt in
        zipWith (toJoinCondition tN ltN) cols (fromMaybe [] lc1) ++ zipWith (toJoinCondition ftN ltN) fCols (fromMaybe [] lc2)
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
    removeSourceCTESchema schema tbl = QualifiedIdentifier (if tbl == sourceCTEName then mempty else schema) tbl

addFiltersOrdersRanges :: ApiRequest -> Either ApiRequestError (ReadRequest -> ReadRequest)
addFiltersOrdersRanges apiRequest = foldr1 (liftA2 (.)) [
    flip (foldr addFilter) <$> filters,
    flip (foldr addOrder) <$> orders,
    flip (foldr addRange) <$> ranges,
    flip (foldr addLogicTree) <$> logicForest
  ]
  {-
  The esence of what is going on above is that we are composing tree functions
  of type (ReadRequest->ReadRequest) that are in (Either ApiRequestError a) context
  -}
  where
    filters :: Either ApiRequestError [(EmbedPath, Filter)]
    filters = mapM pRequestFilter flts
    logicForest :: Either ApiRequestError [(EmbedPath, LogicTree)]
    logicForest = mapM pRequestLogicTree logFrst
    action = iAction apiRequest
    -- there can be no filters on the root table when we are doing insert/update/delete
    (flts, logFrst) =
      case action of
        ActionInvoke _ -> (iFilters apiRequest, iLogic apiRequest)
        ActionRead _   -> (iFilters apiRequest, iLogic apiRequest)
        _              -> join (***) (filter (( "." `isInfixOf` ) . fst)) (iFilters apiRequest, iLogic apiRequest)
    orders :: Either ApiRequestError [(EmbedPath, [OrderTerm])]
    orders = mapM pRequestOrder $ iOrder apiRequest
    ranges :: Either ApiRequestError [(EmbedPath, NonnegRange)]
    ranges = mapM pRequestRange $ M.toList $ iRange apiRequest

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

mutateRequest :: Schema -> TableName -> ApiRequest -> S.Set FieldName -> [FieldName] -> ReadRequest -> Either Response MutateRequest
mutateRequest schema tName apiRequest cols pkCols readReq = mapLeft errorResponseFor $
  case action of
    ActionCreate -> do
        confCols <- case iOnConflict apiRequest of
            Nothing    -> pure pkCols
            Just param -> pRequestOnConflict param
        pure $ Insert qi cols ((,) <$> iPreferResolution apiRequest <*> Just confCols) [] returnings
    ActionUpdate -> Update qi cols <$> combinedLogic <*> pure returnings
    ActionSingleUpsert ->
      (\flts ->
        if null (iLogic apiRequest) &&
           S.fromList (fst <$> iFilters apiRequest) == S.fromList pkCols &&
           not (null (S.fromList pkCols)) &&
           all (\case
              Filter _ (OpExpr False (Op "eq" _)) -> True
              _ -> False) flts
          then Insert qi cols (Just (MergeDuplicates, pkCols)) <$> combinedLogic <*> pure returnings
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
        else returningCols readReq
    filters = map snd <$> mapM pRequestFilter mutateFilters
    logic = map snd <$> mapM pRequestLogicTree logicFilters
    combinedLogic = foldr addFilterToLogicForest <$> logic <*> filters
    -- update/delete filters can be only on the root table
    (mutateFilters, logicFilters) = join (***) onlyRoot (iFilters apiRequest, iLogic apiRequest)
    onlyRoot = filter (not . ( "." `isInfixOf` ) . fst)

returningCols :: ReadRequest -> [FieldName]
returningCols rr@(Node _ forest) = returnings
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
    returnings = S.toList . S.fromList $ fldNames ++ (colName <$> fkCols)

-- Traditional filters(e.g. id=eq.1) are added as root nodes of the LogicTree
-- they are later concatenated with AND in the QueryBuilder
addFilterToLogicForest :: Filter -> [LogicTree] -> [LogicTree]
addFilterToLogicForest flt lf = Stmnt flt : lf
