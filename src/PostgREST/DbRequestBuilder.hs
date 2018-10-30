{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.DbRequestBuilder (
  readRequest
, mutateRequest
, fieldNames
) where

import           Control.Applicative
import           Control.Arrow             ((***))
import           Control.Lens.Getter       (view)
import           Control.Lens.Tuple        (_1)
import qualified Data.ByteString.Char8     as BS
import           Data.List                 (delete)
import           Data.Maybe                (fromJust)
import qualified Data.Set                  as S
import           Data.Text                 (isInfixOf)
import           Data.Tree
import           Data.Either.Combinators   (mapLeft)

import           Network.Wai

import           Data.Foldable (foldr1)
import qualified Data.HashMap.Strict       as M

import           PostgREST.ApiRequest   ( ApiRequest(..)
                                        , PreferRepresentation(..)
                                        , Action(..), Target(..)
                                        , PreferRepresentation (..)
                                        )
import           PostgREST.Error           (apiRequestError)
import           PostgREST.Parsers
import           PostgREST.RangeQuery      (NonnegRange, restrictRange, allRange)
import           PostgREST.Types

import           Protolude                hiding (from)
import           Text.Regex.TDFA         ((=~))
import           Unsafe                  (unsafeHead)

readRequest :: Maybe Integer -> [Relation] -> Maybe ProcDescription -> ApiRequest -> Either Response ReadRequest
readRequest maxRows allRels proc apiRequest  =
  mapLeft apiRequestError $
  treeRestrictRange maxRows =<<
  augumentRequestWithJoin schema relations =<<
  addFiltersOrdersRanges apiRequest <*>
  (buildReadRequest <$> pRequestSelect (iSelect apiRequest))
  where
    action = iAction apiRequest
    (schema, rootTableName) = fromJust $ -- Make it safe
      let target = iTarget apiRequest in
      case target of
        (TargetIdent (QualifiedIdentifier s t) ) -> Just (s, t)
        (TargetProc  (QualifiedIdentifier s pName) ) -> Just (s, tName)
          where
            tName = case pdReturnType <$> proc of
              Just (SetOf (Composite qi)) -> qiName qi
              Just (Single (Composite qi)) -> qiName qi
              _ -> pName

        _ -> Nothing

    -- Build tree with a Depth attribute so when a self join occurs we can differentiate the parent and child tables by having
    -- an alias like "table_depth", this is related to issue #987.
    buildReadRequest :: [Tree SelectItem] -> ReadRequest
    buildReadRequest fieldTree =
      let rootDepth = 0
          rootNodeName = if action == ActionRead then rootTableName else sourceCTEName in
      foldr (treeEntry rootDepth) (Node (Select [] [rootNodeName] [] [] [] allRange, (rootNodeName, Nothing, Nothing, Nothing, rootDepth)) []) fieldTree
      where
        treeEntry :: Depth -> Tree SelectItem -> ReadRequest -> ReadRequest
        treeEntry depth (Node fld@((fn, _),_,alias,relationDetail) fldForest) (Node (q, i) rForest) =
          let nxtDepth = succ depth in
          case fldForest of
            [] -> Node (q {select=fld:select q}, i) rForest
            _  -> Node (q, i) $
                  foldr (treeEntry nxtDepth) (Node (Select [] [fn] [] [] [] allRange, (fn, Nothing, alias, relationDetail, nxtDepth)) []) fldForest:rForest

    relations :: [Relation]
    relations = case action of
      ActionCreate   -> fakeSourceRelations ++ allRels
      ActionUpdate   -> fakeSourceRelations ++ allRels
      ActionDelete   -> fakeSourceRelations ++ allRels
      ActionInvoke _ -> fakeSourceRelations ++ allRels
      _       -> allRels
      where fakeSourceRelations = mapMaybe (toSourceRelation rootTableName) allRels

-- in a relation where one of the tables matches "TableName"
-- replace the name to that table with pg_source
-- this "fake" relations is needed so that in a mutate query
-- we can look at the "returning *" part which is wrapped with a "with"
-- as just another table that has relations with other tables
toSourceRelation :: TableName -> Relation -> Maybe Relation
toSourceRelation mt r@(Relation t _ ft _ _ rt _ _)
  | mt == tableName t = Just $ r {relTable=t {tableName=sourceCTEName}}
  | mt == tableName ft = Just $ r {relFTable=t {tableName=sourceCTEName}}
  | Just mt == (tableName <$> rt) = Just $ r {relLinkTable=(\tbl -> tbl {tableName=sourceCTEName}) <$> rt}
  | otherwise = Nothing

treeRestrictRange :: Maybe Integer -> ReadRequest -> Either ApiRequestError ReadRequest
treeRestrictRange maxRows_ request = pure $ nodeRestrictRange maxRows_ `fmap` request
  where
    nodeRestrictRange :: Maybe Integer -> ReadNode -> ReadNode
    nodeRestrictRange m (q@Select {range_=r}, i) = (q{range_=restrictRange m r }, i)

augumentRequestWithJoin :: Schema ->  [Relation] ->  ReadRequest -> Either ApiRequestError ReadRequest
augumentRequestWithJoin schema allRels request =
  addRelations schema allRels Nothing request
  >>= addJoinConditions schema

addRelations :: Schema -> [Relation] -> Maybe ReadRequest -> ReadRequest -> Either ApiRequestError ReadRequest
addRelations schema allRelations parentNode (Node (query, (nodeName, _, alias, relationDetail, depth)) forest) =
  case parentNode of
    Just (Node (Select{from=[parentNodeTable]}, _) _) ->
      let newFrom r = (\tName -> if tName == nodeName then tableName (relTable r) else tName) <$> from query
          newReadNode = (\r -> (query{from=newFrom r}, (nodeName, Just r, alias, Nothing, depth))) <$> rel
          rel :: Either ApiRequestError Relation
          rel = note (NoRelationBetween parentNodeTable nodeName) $
                findRelation schema allRelations nodeName parentNodeTable relationDetail in
      Node <$> newReadNode <*> (updateForest . hush $ Node <$> newReadNode <*> pure forest)
    _ ->
      let rn = (query, (nodeName, Just r, alias, Nothing, depth))
          r = Relation t [] t [] Root Nothing Nothing Nothing
          t = Table schema nodeName Nothing True in -- !!! TODO find another way to get the table from the query
      Node rn <$> updateForest (Just $ Node rn forest)
  where
    updateForest :: Maybe ReadRequest -> Either ApiRequestError [ReadRequest]
    updateForest rq = mapM (addRelations schema allRelations rq) forest

findRelation :: Schema -> [Relation] -> NodeName -> TableName -> Maybe RelationDetail -> Maybe Relation
findRelation schema allRelations nodeTableName parentNodeTableName relationDetail =
  find (\Relation{relTable, relColumns, relFTable, relFColumns, relType, relLinkTable} ->
    -- Both relation ends need to be on the exposed schema
    schema == tableSchema relTable && schema == tableSchema relFTable &&
    case relationDetail of
      Nothing ->

        -- (request)        => projects { ..., clients{...} }
        -- will match
        -- (relation type)  => parent
        -- (entity)         => clients  {id}
        -- (foriegn entity) => projects {client_id}
        (
          nodeTableName == tableName relTable && -- match relation table name
          parentNodeTableName == tableName relFTable -- match relation foreign table name
        ) ||

        -- (request)        => projects { ..., client_id{...} }
        -- will match
        -- (relation type)  => parent
        -- (entity)         => clients  {id}
        -- (foriegn entity) => projects {client_id}
        (
          parentNodeTableName == tableName relFTable &&
          length relFColumns == 1 &&
          -- match common foreign key names(table_name_id, table_name_fk) to table_name
          (toS ("^" <> colName (unsafeHead relFColumns) <> "_?(?:|[iI][dD]|[fF][kK])$") :: BS.ByteString) =~ (toS nodeTableName :: BS.ByteString)
        )

        -- (request)        => project_id { ..., client_id{...} }
        -- will match
        -- (relation type)  => parent
        -- (entity)         => clients  {id}
        -- (foriegn entity) => projects {client_id}
        -- this case works becasue before reaching this place
        -- addRelation will turn project_id to project so the above condition will match

      Just rd ->

        -- (request)        => clients { ..., projects.client_id{...} }
        -- will match
        -- (relation type)  => child
        -- (entity)         => clients  {id}
        -- (foriegn entity) => projects {client_id}
        (
          relType == Child &&
          nodeTableName == tableName relTable && -- match relation table name
          parentNodeTableName == tableName relFTable && -- match relation foreign table name
          length relColumns == 1 &&
          rd == colName (unsafeHead relColumns)
        ) ||

        -- (request)        => message { ..., person_detail.sender{...} }
        -- will match
        -- (relation type)  => parent
        -- (entity)         => message  {sender}
        -- (foriegn entity) => person_detail {id}
        (
          relType == Parent &&
          nodeTableName == tableName relTable && -- match relation table name
          parentNodeTableName == tableName relFTable && -- match relation foreign table name
          length relFColumns == 1 &&
          rd == colName (unsafeHead relFColumns)
        ) ||

        -- (request)        => tasks { ..., users.tasks_users{...} }
        -- will match
        -- (relation type)  => many
        -- (entity)         => users
        -- (foriegn entity) => tasks
        (
          relType == Many &&
          nodeTableName == tableName relTable && -- match relation table name
          parentNodeTableName == tableName relFTable && -- match relation foreign table name
          rd == tableName (fromJust relLinkTable)
        )
  ) allRelations

addJoinConditions :: Schema -> ReadRequest -> Either ApiRequestError ReadRequest
addJoinConditions schema (Node node@(query, nodeProps@(_, relation, _, _, _)) forest) =
  case relation of
    Just Relation{relType=Root} -> Node node  <$> updatedForest -- this is the root node
    Just rel@Relation{relType=Parent} -> Node (augmentQuery rel, nodeProps) <$> updatedForest
    Just rel@Relation{relType=Child} -> Node (augmentQuery rel, nodeProps) <$> updatedForest
    Just rel@Relation{relType=Many, relLinkTable=(Just linkTable)} ->
      let rq = augmentQuery rel in
      Node (rq{from=tableName linkTable:from rq}, nodeProps) <$> updatedForest
    _ -> Left UnknownRelation
  where
    updatedForest = mapM (addJoinConditions schema) forest
    augmentQuery rel = foldr addJoinCond query (getJoinConditions rel)
    addJoinCond :: JoinCondition -> ReadQuery -> ReadQuery
    addJoinCond jc rq@Select{joinConditions=jcs} = rq{joinConditions=jc:jcs}

getJoinConditions :: Relation -> [JoinCondition]
getJoinConditions (Relation Table{tableSchema=tSchema, tableName=tN} cols Table{tableName=ftN} fCols typ lt lc1 lc2) =
  if | typ == Child || typ == Parent ->
        zipWith (toJoinCondition tN ftN) cols fCols
     | typ == Many ->
        let ltN = maybe "" tableName lt in
        zipWith (toJoinCondition tN ltN) cols (fromMaybe [] lc1) ++ zipWith (toJoinCondition ftN ltN) fCols (fromMaybe [] lc2)
     | typ == Root -> witness
  where
    toJoinCondition :: Text -> Text -> Column -> Column -> JoinCondition
    toJoinCondition tb ftb c fc =
      JoinCondition (QualifiedIdentifier tSchema tb, Nothing, colName c)
                    (QualifiedIdentifier tSchema ftb, Nothing, colName fc)

addFiltersOrdersRanges :: ApiRequest -> Either ApiRequestError (ReadRequest -> ReadRequest)
addFiltersOrdersRanges apiRequest = foldr1 (liftA2 (.)) [
    flip (foldr addFilter) <$> filters,
    flip (foldr addOrder) <$> orders,
    flip (foldr addRange) <$> ranges,
    flip (foldr addLogicTree) <$> logicForest
  ]
  {-
  The esence of what is going on above is that we are composing tree functions
  of type (ReadRequest->ReadRequest) that are in (Either ParseError a) context
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
        ActionRead     -> (iFilters apiRequest, iLogic apiRequest)
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

mutateRequest :: ApiRequest -> TableName -> [Text] -> [FieldName] -> Either Response MutateRequest
mutateRequest apiRequest tName pkCols fldNames = mapLeft apiRequestError $
  case action of
    ActionCreate -> Right $ Insert tName pkCols payload (iPreferResolution apiRequest) [] returnings
    ActionUpdate -> Update tName payload <$> combinedLogic <*> pure returnings
    ActionSingleUpsert ->
      (\flts ->
        if null (iLogic apiRequest) &&
           S.fromList (fst <$> iFilters apiRequest) == S.fromList pkCols &&
           not (null (S.fromList pkCols)) &&
           all (\case
              Filter _ (OpExpr False (Op "eq" _)) -> True
              _ -> False) flts
          then Insert tName pkCols payload (Just MergeDuplicates) <$> combinedLogic <*> pure returnings
        else
          Left InvalidFilters) =<< filters
    ActionDelete -> Delete tName <$> combinedLogic <*> pure returnings
    _            -> Left UnsupportedVerb
  where
    action = iAction apiRequest
    payload = fromJust $ iPayload apiRequest
    returnings = if iPreferRepresentation apiRequest == None then [] else fldNames
    filters = map snd <$> mapM pRequestFilter mutateFilters
    logic = map snd <$> mapM pRequestLogicTree logicFilters
    combinedLogic = foldr addFilterToLogicForest <$> logic <*> filters
    -- update/delete filters can be only on the root table
    (mutateFilters, logicFilters) = join (***) onlyRoot (iFilters apiRequest, iLogic apiRequest)
    onlyRoot = filter (not . ( "." `isInfixOf` ) . fst)

fieldNames :: ReadRequest -> [FieldName]
fieldNames (Node (sel, _) forest) =
  map (fst . view _1) (select sel) ++ map colName fks
  where
    fks = concatMap (fromMaybe [] . f) forest
    f (Node (_, (_, Just Relation{relFColumns=cols, relType=Parent}, _, _, _)) _) = Just cols
    f _ = Nothing

-- Traditional filters(e.g. id=eq.1) are added as root nodes of the LogicTree
-- they are later concatenated with AND in the QueryBuilder
addFilterToLogicForest :: Filter -> [LogicTree] -> [LogicTree]
addFilterToLogicForest flt lf = Stmnt flt : lf
