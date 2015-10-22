{-# LANGUAGE TupleSections #-}
module PostgREST.QueryBuilder
where


import           Control.Error
import           Data.List         (find)
import           Data.Monoid
import           Data.Text         hiding (filter, find, foldr, head, last, map,
                                    null, zipWith)
import           Control.Applicative
import           Data.Tree
import           PostgREST.PgQuery (fromQi,
                                    pgFmtIdent, pgFmtLit, pgFmtOperator,
                                    pgFmtValue, whiteList, insertableValue, orderF)
import           PostgREST.Types
--import qualified Data.Vector       as V (empty)
--import qualified Hasql.Backend     as B

findRelation :: [Relation] -> Text -> Text -> Text -> Maybe Relation
findRelation allRelations s t1 t2 =
  find (\r -> s == relSchema r && t1 == relTable r && t2 == relFTable r) allRelations



addRelations :: Text -> [Relation] -> Maybe ApiRequest -> ApiRequest -> Either Text ApiRequest
addRelations schema allRelations parentNode node@(Node n@(query, (table, _)) forest) =
  case parentNode of
    Nothing -> Node (query, (table, Nothing)) <$> updatedForest
    (Just (Node (_, (parentTable, _)) _)) -> Node <$> (addRel n <$> rel) <*> updatedForest
      where
        rel = note ("no relation between " <> table <> " and " <> parentTable)
            $  findRelation allRelations schema table parentTable
           <|> findRelation allRelations schema parentTable table
        addRel :: (Query, (NodeName, Maybe Relation)) -> Relation -> (Query, (NodeName, Maybe Relation))
        addRel (q, (t, _)) r = (q, (t, Just r))
  where
    updatedForest = mapM (addRelations schema allRelations (Just node)) forest

getJoinConditions :: Relation -> [Filter]
getJoinConditions (Relation s t cs ft fcs typ lt lc1 lc2) =
  case typ of
    Child  -> zipWith (toFilter t ft) cs fcs
    Parent -> zipWith (toFilter t ft) cs fcs
    Many   -> zipWith (toFilter t (fromMaybe "" lt)) cs (fromMaybe [] lc1) ++ zipWith (toFilter ft (fromMaybe "" lt)) fcs (fromMaybe [] lc2)
  where
    toFilter :: Text -> Text -> FieldName -> FieldName -> Filter
    toFilter tb ftb c fc = Filter (c, Nothing) "=" (VForeignKey (QualifiedIdentifier s tb) (ForeignKey ftb fc))

addJoinConditions :: Text -> [Column] -> ApiRequest -> Either Text ApiRequest
addJoinConditions schema allColumns (Node (query, (t, r)) forest) =
  case r of
    Nothing -> Node (updatedQuery, (t, r))  <$> updatedForest -- this is the root node
    Just rel@(Relation{relType=Child}) -> Node (addCond updatedQuery (getJoinConditions rel),(t,r)) <$> updatedForest
    Just (Relation{relType=Parent}) -> Node (updatedQuery, (t,r)) <$> updatedForest
    Just rel@(Relation{relType=Many, relLTable=(Just linkTable)}) ->
      Node (qq, (t, r)) <$> updatedForest
      where
         q = addCond updatedQuery (getJoinConditions rel)
         qq = q{from=linkTable:from q}
    _ -> Left "unknow relation"
  where
    -- add parentTable and parentJoinConditions to the query
    updatedQuery = foldr (flip addCond) (query{from = parentTables ++ from query}) parentJoinConditions
      where
        parentJoinConditions = map (getJoinConditions.snd) parents
        parentTables = map fst parents
        parents = mapMaybe (getParents.rootLabel) forest
        getParents (_, (tbl, Just rel@(Relation{relType=Parent}))) = Just (tbl, rel)
        getParents _ = Nothing
    updatedForest = mapM (addJoinConditions schema allColumns) forest
    addCond q con = q{where_=con ++ where_ q}

-- requestToCountQuery :: Text -> ApiRequest -> PStmt
-- requestToCountQuery schema (Node (Select _ _ conditions _, (mainTbl, _)) _) =
--   B.Stmt query V.empty True
--   where
--     query = Data.Text.unwords [
--       "SELECT pg_catalog.count(1)",
--       "FROM ", fromQi $ QualifiedIdentifier schema mainTbl,
--       ("WHERE " <> intercalate " AND " ( map (pgFmtCondition (QualifiedIdentifier schema mainTbl)) localConditions )) `emptyOnNull` localConditions
--       ]
--     emptyOnNull val x = if null x then "" else val
--     localConditions = filter fn conditions
--       where
--         fn  (Filter{value=VText _}) = True
--         fn  (Filter{value=VForeignKey _ _}) = False

--requestToQuery :: Text -> ApiRequest -> PStmt
requestToQuery :: Text -> ApiRequest -> Text
requestToQuery schema (Node (Select colSelects tbls conditions ord, (mainTbl, _)) forest) =
  --orderT (fromMaybe [] ord)  query
  query
  where
    --query = B.Stmt qStr V.empty True
    --qStr = Data.Text.unwords [
    query = Data.Text.unwords [
      ("WITH " <> intercalate ", " withs) `emptyOnNull` withs,
      "SELECT ", intercalate ", " (map (pgFmtSelectItem (QualifiedIdentifier schema mainTbl)) colSelects ++ selects),
      "FROM ", intercalate ", " (map (fromQi . QualifiedIdentifier schema) tbls),
      ("WHERE " <> intercalate " AND " ( map (pgFmtCondition (QualifiedIdentifier schema mainTbl) ) conditions )) `emptyOnNull` conditions,
      orderF (fromMaybe [] ord)
      ]
    emptyOnNull val x = if null x then "" else val
    (withs, selects) = foldr getQueryParts ([],[]) forest
    getQueryParts :: Tree ApiNode -> ([Text], [Text]) -> ([Text], [Text])
    getQueryParts (Node n@(_, (table, Just (Relation {relType=Child}))) forst) (w,s) = (w,sel:s)
      where
        sel = "("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> ") AS " <> table
           --where (B.Stmt subquery _ _) = requestToQuery schema (Node n forst)
           where subquery = requestToQuery schema (Node n forst)

    getQueryParts (Node n@(_, (table, Just (Relation {relType=Parent}))) forst) (w,s) = (wit:w,sel:s)
      where
        sel = "row_to_json(" <> table <> ".*) AS "<>table --TODO must be singular
        wit = table <> " AS ( " <> subquery <> " )"
          --where (B.Stmt subquery _ _) = requestToQuery schema (Node n forst)
          where subquery = requestToQuery schema (Node n forst)

    getQueryParts (Node n@(_, (table, Just (Relation {relType=Many}))) forst) (w,s) = (w,sel:s)
      where
        sel = "("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> ") AS " <> table
           --where (B.Stmt subquery _ _) = requestToQuery schema (Node n forst)
           where subquery = requestToQuery schema (Node n forst)

    -- the following is just to remove the warning
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts (Node (_,(_,Nothing)) _) _ = undefined
requestToQuery schema (Node (Insert _ flds vals, (mainTbl, _)) _) =
  query
  where
    --query = B.Stmt qStr V.empty True
    qi = QualifiedIdentifier schema mainTbl
    --qStr = Data.Text.unwords [
    query = Data.Text.unwords [
      "INSERT INTO ", fromQi qi,
      " (" <> intercalate ", " (map (pgFmtIdent . fst) flds) <> ") ",
      "VALUES " <> intercalate ", "
        ( map (\v ->
            "(" <>
            intercalate ", " ( map insertableValue v ) <>
            ")"
          ) vals
        ),
      "RETURNING " <> fromQi qi <> ".*"
      ]
    -- ("insert into " <> fromQi t <> " (" <>
    --   T.intercalate ", " (V.toList $ V.map pgFmtIdent cols) <>
    --   ") values "
    --   <> T.intercalate ", "
    --     (V.toList $ V.map (\v -> "("
    --         <> T.intercalate ", " (V.toList $ V.map insertableValue v)
    --         <> ")"
    --       ) vals
    --     )
    --   <> " returning row_to_json(" <> fromQi t <> ".*)")


pgFmtCondition :: QualifiedIdentifier -> Filter -> Text
pgFmtCondition table (Filter (col,jp) ops val) =
  notOp <> " " <> sqlCol  <> " " <> pgFmtOperator opCode <> " " <>
    if opCode `elem` ["is","isnot"] then whiteList (getInner val) else sqlValue
  where
    headPredicate:rest = split (=='.') ops
    hasNot caseTrue caseFalse = if headPredicate == "not" then caseTrue else caseFalse
    opCode      = hasNot (head rest) headPredicate
    notOp       = hasNot headPredicate ""
    sqlCol = case val of
      VText _ -> pgFmtColumn table col <> pgFmtJsonPath jp
      VForeignKey qi _ -> pgFmtColumn qi col
    sqlValue = valToStr val
    getInner v = case v of
      VText s -> s
      _      -> ""
    valToStr v = case v of
      VText s -> pgFmtValue opCode s
      VForeignKey (QualifiedIdentifier s _) (ForeignKey ft fc) -> pgFmtColumn (QualifiedIdentifier s ft) fc

pgFmtColumn :: QualifiedIdentifier -> Text -> Text
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c = fromQi table <> "." <> pgFmtIdent c

pgFmtJsonPath :: Maybe JsonPath -> Text
pgFmtJsonPath (Just [x]) = "->>" <> pgFmtLit x
pgFmtJsonPath (Just (x:xs)) = "->" <> pgFmtLit x <> pgFmtJsonPath ( Just xs )
pgFmtJsonPath _ = ""

pgFmtTable :: Table -> Text
pgFmtTable Table{tableSchema=s, tableName=n} = fromQi $ QualifiedIdentifier s n

pgFmtField :: QualifiedIdentifier -> Field -> Text
pgFmtField table (c, jp) = pgFmtColumn table c <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> Text
pgFmtSelectItem table (f@(_, jp), Nothing) = pgFmtField table f <> asJsonPath jp
pgFmtSelectItem table (f@(_, jp), Just cast ) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> asJsonPath jp

asJsonPath :: Maybe JsonPath -> Text
asJsonPath Nothing = ""
asJsonPath (Just xx) = " AS " <> last xx
