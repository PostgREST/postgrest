{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields    #-}
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
import           PostgREST.RangeQuery      (NonnegRange, restrictRange)
import           PostgREST.QueryBuilder (getJoinConditions, sourceCTEName)
import           PostgREST.Types

import           Protolude                hiding (from, dropWhile, drop)
import           Text.Regex.TDFA         ((=~))
import           Unsafe                  (unsafeHead)

readRequest :: Maybe Integer -> [Relation] -> M.HashMap Text ProcDescription -> ApiRequest -> Either Response ReadRequest
readRequest maxRows allRels allProcs apiRequest  =
  mapLeft apiRequestError $
  treeRestrictRange maxRows =<<
  augumentRequestWithJoin schema relations =<<
  parseReadRequest
  where
    (schema, rootTableName) = fromJust $ -- Make it safe
      let target = iTarget apiRequest in
      case target of
        (TargetIdent (QualifiedIdentifier s t) ) -> Just (s, t)
        (TargetProc  (QualifiedIdentifier s proc) ) -> Just (s, tName)
          where
            retType = pdReturnType <$> M.lookup proc allProcs
            tName = case retType of
              Just (SetOf (Composite qi)) -> qiName qi
              Just (Single (Composite qi)) -> qiName qi
              _ -> proc

        _ -> Nothing

    action :: Action
    action = iAction apiRequest

    parseReadRequest :: Either ApiRequestError ReadRequest
    parseReadRequest = addFiltersOrdersRanges apiRequest <*>
      pRequestSelect rootName selStr
      where
        selStr = iSelect apiRequest
        rootName = if action == ActionRead
          then rootTableName
          else sourceCTEName

    relations :: [Relation]
    relations = case action of
      ActionCreate -> fakeSourceRelations ++ allRels
      ActionUpdate -> fakeSourceRelations ++ allRels
      ActionDelete -> fakeSourceRelations ++ allRels
      ActionInvoke -> fakeSourceRelations ++ allRels
      _       -> allRels
      where fakeSourceRelations = mapMaybe (toSourceRelation rootTableName) allRels -- see comment in toSourceRelation

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
addRelations schema allRelations parentNode (Node readNode@(query, (name, _, alias)) forest) =
  case parentNode of
    (Just (Node (Select{from=[parentNodeTable]}, (_, _, _)) _)) ->
      Node <$> readNode' <*> forest'
      where
        forest' = updateForest $ hush node'
        node' = Node <$> readNode' <*> pure forest
        readNode' = addRel readNode <$> rel
        rel :: Either ApiRequestError Relation
        rel = note (NoRelationBetween parentNodeTable name)
            $ findRelation schema name parentNodeTable

            where
              findRelation s nodeTableName parentNodeTableName =
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
                where n `colMatches` rc = (toS ("^" <> rc <> "_?(?:|[iI][dD]|[fF][kK])$") :: BS.ByteString) =~ (toS n :: BS.ByteString)
        addRel :: (ReadQuery, (NodeName, Maybe Relation, Maybe Alias)) -> Relation -> (ReadQuery, (NodeName, Maybe Relation, Maybe Alias))
        addRel (query', (n, _, a)) r = (query' {from=fromRelation}, (n, Just r, a))
          where fromRelation = map (\t -> if t == n then tableName (relTable r) else t) (from query')

    _ -> n' <$> updateForest (Just (n' forest))
      where
        n' = Node (query, (name, Just r, alias))
        t = Table schema name Nothing True -- !!! TODO find another way to get the table from the query
        r = Relation t [] t [] Root Nothing Nothing Nothing
  where
    updateForest :: Maybe ReadRequest -> Either ApiRequestError [ReadRequest]
    updateForest n = mapM (addRelations schema allRelations n) forest

addJoinConditions :: Schema -> ReadRequest -> Either ApiRequestError ReadRequest
addJoinConditions schema (Node nn@(query, (n, r, a)) forest) =
  case r of
    Just Relation{relType=Root} -> Node nn  <$> updatedForest -- this is the root node
    Just rel@Relation{relType=Child} -> Node (addCond query (getJoinConditions rel),(n,r,a)) <$> updatedForest
    Just Relation{relType=Parent} -> Node nn <$> updatedForest
    Just rel@Relation{relType=Many, relLTable=(Just linkTable)} ->
      Node (qq, (n, r, a)) <$> updatedForest
      where
         query' = addCond query (getJoinConditions rel)
         qq = query'{from=tableName linkTable : from query'}
    _ -> Left UnknownRelation
  where
    updatedForest = mapM (addJoinConditions schema) forest
    addCond query' con = query'{flt_=con ++ flt_ query'}

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
    (flts, logFrst)
      | action == ActionRead || action == ActionInvoke = (iFilters apiRequest, iLogic apiRequest)
      | otherwise = join (***) (filter (( "." `isInfixOf` ) . fst)) (iFilters apiRequest, iLogic apiRequest)
    orders :: Either ApiRequestError [(EmbedPath, [OrderTerm])]
    orders = mapM pRequestOrder $ iOrder apiRequest
    ranges :: Either ApiRequestError [(EmbedPath, NonnegRange)]
    ranges = mapM pRequestRange $ M.toList $ iRange apiRequest

addFilterToNode :: Filter -> ReadRequest -> ReadRequest
addFilterToNode flt (Node (q@Select {flt_=flts}, i) f) = Node (q {flt_=flt:flts}, i) f

addFilter :: (EmbedPath, Filter) -> ReadRequest -> ReadRequest
addFilter = addProperty addFilterToNode

addOrderToNode :: [OrderTerm] -> ReadRequest -> ReadRequest
addOrderToNode o (Node (q,i) f) = Node (q{order=Just o}, i) f

addOrder :: (EmbedPath, [OrderTerm]) -> ReadRequest -> ReadRequest
addOrder = addProperty addOrderToNode

addRangeToNode :: NonnegRange -> ReadRequest -> ReadRequest
addRangeToNode r (Node (q,i) f) = Node (q{range_=r}, i) f

addRange :: (EmbedPath, NonnegRange) -> ReadRequest -> ReadRequest
addRange = addProperty addRangeToNode

addLogicTreeToNode :: LogicTree -> ReadRequest -> ReadRequest
addLogicTreeToNode t (Node (q@Select{logic=l},i) f) = Node (q{logic=t:l}::ReadQuery, i) f

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
            fnd (Node (_,(n,_,_)) _) = n == name

-- in a relation where one of the tables mathces "TableName"
-- replace the name to that table with pg_source
-- this "fake" relations is needed so that in a mutate query
-- we can look a the "returning *" part which is wrapped with a "with"
-- as just another table that has relations with other tables
toSourceRelation :: TableName -> Relation -> Maybe Relation
toSourceRelation mt r@(Relation t _ ft _ _ rt _ _)
  | mt == tableName t = Just $ r {relTable=t {tableName=sourceCTEName}}
  | mt == tableName ft = Just $ r {relFTable=t {tableName=sourceCTEName}}
  | Just mt == (tableName <$> rt) = Just $ r {relLTable=(\tbl -> tbl {tableName=sourceCTEName}) <$> rt}
  | otherwise = Nothing

mutateRequest :: ApiRequest -> [FieldName] -> Either Response MutateRequest
mutateRequest apiRequest fldNames = mapLeft apiRequestError $
  case action of
    ActionCreate -> Right $ Insert rootTableName payload returnings
    ActionUpdate -> Update rootTableName <$> pure payload <*> filters <*> logic_ <*> pure returnings
    ActionDelete -> Delete rootTableName <$> filters <*> logic_ <*> pure returnings
    _        -> Left UnsupportedVerb
  where
    action = iAction apiRequest
    payload = fromJust $ iPayload apiRequest
    rootTableName = -- TODO: Make it safe
      let target = iTarget apiRequest in
      case target of
        (TargetIdent (QualifiedIdentifier _ t) ) -> t
        _ -> undefined
    returnings = if iPreferRepresentation apiRequest == None then [] else fldNames
    filters = map snd <$> mapM pRequestFilter mutateFilters
    logic_ = map snd <$> mapM pRequestLogicTree logicFilters
    -- update/delete filters can be only on the root table
    mutateFilters = onlyRoot $ iFilters apiRequest
    logicFilters = onlyRoot $ iLogic apiRequest
    onlyRoot = filter (not . ( "." `isInfixOf` ) . fst)

fieldNames :: ReadRequest -> [FieldName]
fieldNames (Node (sel, _) forest) =
  map (fst . view _1) (select sel) ++ map colName fks
  where
    fks = concatMap (fromMaybe [] . f) forest
    f (Node (_, (_, Just Relation{relFColumns=cols, relType=Parent}, _)) _) = Just cols
    f _ = Nothing
