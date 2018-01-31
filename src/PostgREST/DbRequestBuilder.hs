{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE LambdaCase #-}
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

import           Protolude                hiding (from, dropWhile, drop)
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

    -- Build tree with a Level attribute so when an embed occurs and the parent node has the same name as the child we can differentiate them by having
    -- an alias like "node_lvl", this is related to issue #987.
    buildReadRequest :: [Tree SelectItem] -> ReadRequest
    buildReadRequest fieldTree =
      let rootLvl = 1
          rootNodeName = if action == ActionRead then rootTableName else sourceCTEName in
      foldr (treeEntry rootLvl) (Node (Select [] [rootNodeName] [] [] [] allRange, (rootNodeName, Nothing, Nothing, Nothing, rootLvl)) []) fieldTree
      where
        treeEntry :: Level -> Tree SelectItem -> ReadRequest -> ReadRequest
        treeEntry lvl (Node fld@((fn, _),_,alias,relationDetail) fldForest) (Node (q, i) rForest) =
          let nxtLvl = succ lvl in
          case fldForest of
            [] -> Node (q {select=fld:select q}, i) rForest
            _  -> Node (q, i) $
                  foldr (treeEntry nxtLvl) (Node (Select [] [fn] [] [] [] allRange, (fn, Nothing, alias, relationDetail, nxtLvl)) []) fldForest:rForest

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
-- we can look a the "returning *" part which is wrapped with a "with"
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
addRelations schema allRelations parentNode (Node readNode@(query, (name, _, alias, relationDetail, level)) forest) =
  case parentNode of
    (Just (Node (Select{from=[parentNodeTable]}, _) _)) ->
      Node <$> readNode' <*> forest'
      where
        forest' = updateForest $ hush node'
        node' = Node <$> readNode' <*> pure forest
        readNode' = addRel readNode <$> rel
        rel :: Either ApiRequestError Relation
        rel = note (NoRelationBetween parentNodeTable name)
            $ findRelation schema name parentNodeTable relationDetail
            where
              
              findRelation s nodeTableName parentNodeTableName Nothing =
                find (\r ->
                  s == tableSchema (relTable r) && -- match schema for relation table
                  s == tableSchema (relFTable r) && -- match schema for relation foriegn table
                  (

                    -- (request)        => projects { ..., clients{...} }
                    -- will match
                    -- (relation type)  => parent
                    -- (entity)         => clients  {id}
                    -- (foriegn entity) => projects {client_id}
                    (
                      nodeTableName == tableName (relTable r) && -- match relation table name
                      parentNodeTableName == tableName (relFTable r) -- match relation foreign table name
                    ) ||


                    -- (request)        => projects { ..., client_id{...} }
                    -- will match
                    -- (relation type)  => parent
                    -- (entity)         => clients  {id}
                    -- (foriegn entity) => projects {client_id}
                    (
                      parentNodeTableName == tableName (relFTable r) &&
                      length (relFColumns r) == 1 &&
                      nodeTableName `colMatches` (colName . unsafeHead . relFColumns) r
                    )

                    -- (request)        => project_id { ..., client_id{...} }
                    -- will match
                    -- (relation type)  => parent
                    -- (entity)         => clients  {id}
                    -- (foriegn entity) => projects {client_id}
                    -- this case works becasue before reaching this place
                    -- addRelation will turn project_id to project so the above condition will match
                  )
                ) allRelations
              
              findRelation s nodeTableName parentNodeTableName (Just rd) = 
                find (\r ->
                  s == tableSchema (relTable r) && -- match schema for relation table
                  s == tableSchema (relFTable r) && -- match schema for relation foriegn table
                  (

                    -- (request)        => clients { ..., project.client_id{...} }
                    -- will match
                    -- (relation type)  => parent
                    -- (entity)         => clients  {id}
                    -- (foriegn entity) => projects {client_id}
                    (
                      nodeTableName == tableName (relTable r) && -- match relation table name
                      parentNodeTableName == tableName (relFTable r) && -- && -- match relation foreign table name
                      length (relColumns r) == 1 &&
                      rd == (colName . unsafeHead . relColumns) r
                    ) 
                    ||


                    -- (request)        => tasks { ..., users.tasks_users{...} }
                    -- will match
                    -- (relation type)  => many
                    -- (entity)         => users
                    -- (foriegn entity) => tasks
                    (
                      relType r == Many &&
                      nodeTableName == tableName (relTable r) && -- match relation table name
                      parentNodeTableName == tableName (relFTable r) && -- match relation foreign table name
                      rd == tableName (fromJust (relLinkTable r))
                    ) 
                  )
                ) allRelations
              n `colMatches` rc = (toS ("^" <> rc <> "_?(?:|[iI][dD]|[fF][kK])$") :: BS.ByteString) =~ (toS n :: BS.ByteString)
        addRel :: (ReadQuery, (NodeName, Maybe Relation, Maybe Alias, Maybe RelationDetail, Level)) -> Relation -> (ReadQuery, (NodeName, Maybe Relation, Maybe Alias, Maybe RelationDetail, Level))
        addRel (query', (n, _, a, _, lvl)) r = (query' {from=fromRelation}, (n, Just r, a, Nothing, lvl))
          where fromRelation = map (\t -> if t == n then tableName (relTable r) else t) (from query')

    _ -> n' <$> updateForest (Just (n' forest))
      where
        n' = Node (query, (name, Just r, alias, Nothing, level))
        t = Table schema name Nothing True -- !!! TODO find another way to get the table from the query
        r = Relation t [] t [] Root Nothing Nothing Nothing
  where
    updateForest :: Maybe ReadRequest -> Either ApiRequestError [ReadRequest]
    updateForest n = mapM (addRelations schema allRelations n) forest

addJoinConditions :: Schema -> ReadRequest -> Either ApiRequestError ReadRequest
addJoinConditions schema (Node node@(query, nodeProps@(_, relation, _, _, lvl)) forest) =
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
    augmentQuery rel = foldr addJoinCondToReadQuery query (getJoinConds lvl rel)
    addJoinCondToReadQuery jc rq@Select{joinConds=jcs} = rq{joinConds=jc:jcs}

getJoinConds :: Integer -> Relation -> [JoinCond]
getJoinConds level (Relation t cols ft fcs typ lt lc1 lc2) =
  case typ of
    -- JoinCond needs the Level attr to know the tables aliases
    -- The level depends on the sql query structure
    -- Child has the embed as:
    -- SELECT .., COALESCE(SELECT .. FROM ch AS ch_lvl_2 WHERE ch_lvl_2.col = p_lvl_1.col) FROM p AS p_lvl_1
    -- Parent has similar structure regarding the levels
    -- Many has the embed as:
    -- SELECT .., COALESCE(SELECT .. FROM ch AS ch_lvl_2, gch AS gch_lvl_2 WHERE ch_lvl_2.col = gch_lvl_2.col AND p_lvl_1.acol = ch_lvl_2.acol)
    -- FROM p AS p_lvl_1
    Child  -> zipWith (toJoinCond (tN, level) (ftN, level - 1)) cols fcs
    Parent -> zipWith (toJoinCond (tN, level) (ftN, level - 1)) cols fcs
    Many   -> zipWith (toJoinCond (tN, level) (ltN, level)) cols (fromMaybe [] lc1) ++ zipWith (toJoinCond (ftN, level - 1) (ltN, level)) fcs (fromMaybe [] lc2)
    Root   -> undefined
  where
    s = if typ == Parent then "" else tableSchema t
    tN = tableName t
    ftN = tableName ft
    ltN = fromMaybe "" (tableName <$> lt)
    toJoinCond :: (Text, Integer) -> (Text, Integer) -> Column -> Column -> JoinCond
    toJoinCond (tb, tLvl) (ftb, fLvl) c fc = JoinCond (QualifiedIdentifier s tb, colName c, tLvl) (QualifiedIdentifier s ftb, colName fc, fLvl)

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
addProperty f ([], a) n = f a n
addProperty f (path, a) (Node rn forest) =
  case targetNode of
    Nothing -> Node rn forest -- the property is silenty dropped in the Request does not contain the required path
    Just tn -> Node rn (addProperty f (remainingPath, a) tn:restForest)
  where
    targetNodeName:remainingPath = path
    (targetNode,restForest) = splitForest targetNodeName forest
    splitForest :: NodeName -> Forest ReadNode -> (Maybe ReadRequest, Forest ReadNode)
    splitForest name forst =
      case maybeNode of
        Nothing -> (Nothing,forest)
        Just node -> (Just node, delete node forest)
      where
        maybeNode :: Maybe ReadRequest
        maybeNode = find fnd forst
          where
            fnd :: ReadRequest -> Bool
            fnd (Node (_,(n,_,_,_,_)) _) = n == name

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
