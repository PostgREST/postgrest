{-# LANGUAGE OverloadedStrings #-}
module PostgREST.Functions
where


import           Control.Error
import           Data.List         (find)
import           Data.Monoid
import           Data.Text         hiding (filter, find, foldr, head, last, map,
                                    null)

import           Data.Tree
import           PostgREST.PgQuery (PStmt, QualifiedIdentifier (..), fromQi,
                                    orderT, pgFmtIdent, pgFmtLit, pgFmtOperator,
                                    pgFmtValue, whiteList)
import           PostgREST.Types
--import qualified Hasql as H
--import qualified Hasql.Postgres as P
import qualified Data.Vector       as V (empty)
import qualified Hasql.Backend     as B



findColumn :: [Column] -> Text -> Text -> Text -> Either Text Column
findColumn allColumns s t c = note ("no such column: "<>t<>"."<>c) $
  find (\ col -> colSchema col == s && colTable col == t && colName col == c ) allColumns

findTable :: [Table] -> Text -> Text -> Either Text Table
findTable allTables s t = note ("no such table: "<>t) $
  find (\tb-> s == tableSchema tb && t == tableName tb ) allTables

findRelation :: [Relation] -> Text -> Text -> Text -> Maybe Relation
findRelation allRelations s t1 t2 =
  find (\r -> s == relSchema r && t1 == relTable r && t2 == relFTable r) allRelations


addRelations :: Text -> [Relation] -> Maybe ApiRequest -> ApiRequest -> Either Text ApiRequest
addRelations schema allRelations parentNode node@(Node query@(Select {mainTable=table}) forest) =
  case parentNode of
    Nothing -> Node query{relation=Nothing} <$> updatedForest
    (Just (Node (Select{mainTable=parentTable}) _)) -> Node <$> (addRel query <$> rel) <*> updatedForest
      where
        rel = note ("no relation between " <> table <> " and " <> parentTable) $
          findRelation allRelations schema table parentTable
        addRel :: Query -> Relation -> Query
        addRel q r = q{relation = Just r}
  where
    updatedForest = mapM (addRelations schema allRelations (Just node)) forest


addJoinConditions :: Text -> [Column] -> ApiRequest -> Either Text ApiRequest
addJoinConditions schema allColumns (Node query@(Select{relation=r}) forest) =
  case r of
    Nothing -> Node updatedQuery  <$> updatedForest -- this is the root node
    Just rel@(Relation{relType="child"}) -> Node (addCond updatedQuery (getJoinCondition rel)) <$> updatedForest
    Just (Relation{relType="parent"}) -> Node updatedQuery <$> updatedForest
    -- Just (Many relationColumn1 relationColumn2) -> Node <$> pure updatedQuery{qJoinTables=linkTable:qJoinTables updatedQuery, qWhere=cond1:cond2:qWhere updatedQuery} <*> updatedForest
      -- where
      --    cond1 = getJoinCondition relationColumn1
      --    cond2 = getJoinCondition relationColumn2
      --    linkTable = Table "public" (colTable relationColumn1) True
    _ -> Left "unknow relation"
  where
    -- add parentTable and parentJoinConditions to the query
    updatedQuery = foldr (flip addCond) (query{joinTables = parentTables ++ joinTables query}) parentJoinConditions
      where
        parentJoinConditions = map (getJoinCondition.snd) parents
        parentTables = map fst parents
        parents = mapMaybe (getParents.rootLabel) forest
        getParents qq@(Select{relation=(Just rel@(Relation{relType="parent"}))}) = Just (mainTable qq, rel)
        getParents _ = Nothing
    updatedForest = mapM (addJoinConditions schema allColumns) forest
    getJoinCondition rel@(Relation _ _ c _ _ _) = Filter (c, Nothing) "=" (VForeignKey rel)
    addCond q con = q{filters=con:filters q}


requestToCountQuery :: Text -> ApiRequest -> PStmt
requestToCountQuery schema (Node (Select mainTbl _ _ conditions _ _) _) =
  B.Stmt query V.empty True
  where
    query = Data.Text.unwords [
      "SELECT pg_catalog.count(1)",
      "FROM ", fromQi $ QualifiedIdentifier schema mainTbl,
      ("WHERE " <> intercalate " AND " ( map (pgFmtCondition (QualifiedIdentifier schema mainTbl)) localConditions )) `emptyOnNull` localConditions
      ]
    emptyOnNull val x = if null x then "" else val
    localConditions = filter fn conditions
      where
        fn  (Filter{value=VText _}) = True
        fn  (Filter{value=VForeignKey _}) = False


-- main field join filters order rela
requestToQuery :: Text -> ApiRequest -> PStmt
requestToQuery schema (Node (Select mainTbl colSelects tbls conditions ord _) forest) =
  orderT (fromMaybe [] ord)  query
  where
    query = B.Stmt qStr V.empty True
    qStr = Data.Text.unwords [
      ("WITH " <> intercalate ", " withs) `emptyOnNull` withs,
      "SELECT ", intercalate ", " (map (pgFmtSelectItem (QualifiedIdentifier schema mainTbl)) colSelects ++ selects),
      "FROM ", intercalate ", " (map (fromQi . QualifiedIdentifier schema) (mainTbl:tbls)),
      ("WHERE " <> intercalate " AND " ( map (pgFmtCondition (QualifiedIdentifier schema mainTbl) ) conditions )) `emptyOnNull` conditions
      ]
    emptyOnNull val x = if null x then "" else val
    (withs, selects) = foldr getQueryParts ([],[]) forest
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts :: Tree Query -> ([Text], [Text]) -> ([Text], [Text])
    getQueryParts (Node q@(Select{mainTable=table, relation=(Just (Relation {relType="child"}))}) forst) (w,s) = (w,sel:s)
      where
        sel = "("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> ") AS " <> table
           where (B.Stmt subquery _ _) = requestToQuery schema (Node q forst)

    getQueryParts (Node q@(Select{mainTable=table, relation=(Just (Relation{relType="parent"}))}) forst) (w,s) = (wit:w,sel:s)
      where
        sel = "row_to_json(" <> table <> ".*) AS "<>table --TODO must be singular
        wit = table <> " AS ( " <> subquery <> " )"
          where (B.Stmt subquery _ _) = requestToQuery schema (Node q forst)
    -- getQueryParts (Node q@(Select{qMainTable=table, qRelation=(Just (Many _ _))}) forst) (w,s) = (w,sel:s)
    --    where name = tableName table
    --         sel = "("
    --           <> "SELECT array_to_json(array_agg(row_to_json("<>name<>"))) "
    --           <> "FROM (" <> requestToQuery (Node q forst) <> ") " <> name
    --           <> ") AS " <> name
    -- the following is just to remove the warning, maybe relType should not be String?
    getQueryParts (Node (Select{relation=Nothing}) _) _ = undefined
    getQueryParts (Node (Select{relation=(Just (Relation {relType=_}))}) _) _ = undefined

pgFmtCondition :: QualifiedIdentifier -> Filter -> Text
pgFmtCondition table (Filter (col,jp) ops val) =
  notOp <> " " <> pgFmtColumn table col <> pgFmtJsonPath jp  <> " " <> pgFmtOperator opCode <> " " <>
    if opCode `elem` ["is","isnot"] then whiteList (getInner val) else sqlValue
  where
    headPredicate:rest = split (=='.') ops
    hasNot caseTrue caseFalse = if headPredicate == "not" then caseTrue else caseFalse
    opCode      = hasNot (head rest) headPredicate
    notOp       = hasNot headPredicate ""
    sqlValue = valToStr val
    getInner v = case v of
      VText s -> s
      _      -> ""
    valToStr v = case v of
      VText s -> pgFmtValue opCode s
      VForeignKey (Relation{relSchema=s, relFTable=ft, relFColumn=fc}) -> pgFmtColumn (QualifiedIdentifier s ft) fc

pgFmtColumn :: QualifiedIdentifier -> Text -> Text
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c = fromQi table <> "." <> pgFmtIdent c
--pgFmtColumn Star {colSchema=s, colTable=t} = pgFmtIdent s <> "." <> pgFmtIdent t <> ".*"

pgFmtJsonPath :: Maybe JsonPath -> Text
pgFmtJsonPath (Just [x]) = "->>" <> pgFmtLit x
pgFmtJsonPath (Just (x:xs)) = "->" <> pgFmtLit x <> pgFmtJsonPath ( Just xs )
pgFmtJsonPath _ = ""

pgFmtTable :: Table -> Text
pgFmtTable Table{tableSchema=s, tableName=n} = fromQi $ QualifiedIdentifier s n

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> Text
pgFmtSelectItem table ((c, jp), Nothing) = pgFmtColumn table c <> pgFmtJsonPath jp <> asJsonPath jp
pgFmtSelectItem table ((c, jp), Just cast ) = "CAST (" <> pgFmtColumn table c <> pgFmtJsonPath jp <> " AS " <> cast <> " )" <> asJsonPath jp

asJsonPath :: Maybe JsonPath -> Text
asJsonPath Nothing = ""
asJsonPath (Just xx) = " AS " <> last xx
