{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TupleSections        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : PostgREST.QueryBuilder
Description : PostgREST SQL generating functions.

This module provides functions to consume data types that
represent database objects (e.g. Relation, Schema, SqlQuery)
and produces SQL Statements.

Any function that outputs a SQL fragment should be in this module.
-}
module PostgREST.QueryBuilder (
    addRelations
  , addJoinConditions
  , asJson
  , callProc
  , createReadStatement
  , createWriteStatement
  , operators
  , pgFmtIdent
  , pgFmtLit
  , requestToQuery
  , requestToCountQuery
  , sourceCTEName
  , unquoted
  ) where

import qualified Hasql                   as H
import qualified Hasql.Backend           as B
import qualified Hasql.Postgres          as P

import qualified Data.Aeson              as JSON

import           PostgREST.RangeQuery    (NonnegRange, rangeLimit, rangeOffset)
import           Control.Error           (note, fromMaybe, mapMaybe)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (find, (\\))
import           Data.Monoid             ((<>))
import           Data.Text               (Text, intercalate, unwords, replace, isInfixOf, toLower, split)
import qualified Data.Text as T          (map, takeWhile)
import           Data.String.Conversions (cs)
import           Control.Applicative     (empty, (<|>))
import           Control.Monad           (join)
import           Data.Tree               (Tree(..))
import qualified Data.Vector as V
import           PostgREST.Types
import qualified Data.Map as M
import           Text.Regex.TDFA         ((=~))
import qualified Data.ByteString.Char8   as BS
import           Data.Scientific         ( FPFormat (..)
                                         , formatScientific
                                         , isInteger
                                         )
import           Prelude hiding          (unwords)
import           PostgREST.ApiRequest    (PreferRepresentation (..))

type PStmt = H.Stmt P.Postgres
instance Monoid PStmt where
  mappend (B.Stmt query params prep) (B.Stmt query' params' prep') =
    B.Stmt (query <> query') (params <> params') (prep && prep')
  mempty = B.Stmt "" empty True
type StatementT = PStmt -> PStmt

createReadStatement :: SqlQuery -> SqlQuery -> NonnegRange -> Bool -> Bool -> Bool -> B.Stmt P.Postgres
createReadStatement selectQuery countQuery range isSingle countTotal asCsv =
  B.Stmt (
    "WITH " <> sourceCTEName <> " AS (" <> selectQuery <> ") " <>
    "SELECT " <> intercalate ", " [
      countResultF <> " AS total_result_set",
      "pg_catalog.count(t) AS page_total",
      "null AS header",
      bodyF <> " AS body"
    ] <>
    " FROM ( SELECT * FROM " <> sourceCTEName <> " " <> limitF range <> ") t"
  ) V.empty True
  where
    countResultF = if countTotal then "("<>countQuery<>")" else "null"
    bodyF
      | asCsv = asCsvF
      | isSingle = asJsonSingleF
      | otherwise = asJsonF

createWriteStatement :: TableName -> SqlQuery -> SqlQuery -> Bool -> PreferRepresentation ->
                        [Text] -> Bool -> Payload -> B.Stmt P.Postgres
createWriteStatement _ _ _ _ _ _ _ (PayloadParseError _) = undefined
createWriteStatement _ _ mutateQuery _ None
                     _ _ (PayloadJSON (UniformObjects rows)) =
  B.Stmt (
    "WITH " <> sourceCTEName <> " AS (" <> mutateQuery <> ") " <>
    "SELECT null, 0, null, null"
  ) (V.singleton . B.encodeValue . JSON.Array . V.map JSON.Object $ rows) True
createWriteStatement table _ mutateQuery isSingle HeadersOnly
                     pKeys _ (PayloadJSON (UniformObjects rows)) =
  B.Stmt (
    "WITH " <> sourceCTEName <> " AS (" <> mutateQuery <> " RETURNING " <> table <> ".*" <> ") " <>
    "SELECT " <> intercalate ", " [
      "null AS total_result_set",
      "pg_catalog.count(t) AS page_total",
      if isSingle then locationF pKeys else "null",
      "null"
    ] <>
    " FROM (SELECT 1 FROM " <> sourceCTEName <> ") t"
  ) (V.singleton . B.encodeValue . JSON.Array . V.map JSON.Object $ rows) True
createWriteStatement table selectQuery mutateQuery isSingle Full
                     pKeys asCsv (PayloadJSON (UniformObjects rows)) =
  B.Stmt (
    "WITH " <> sourceCTEName <> " AS (" <> mutateQuery <> " RETURNING " <> table <> ".*" <> ") " <>
    "SELECT " <> intercalate ", " [
      "null AS total_result_set", -- when updateing it does not make sense
      "pg_catalog.count(t) AS page_total",
      if isSingle then locationF pKeys else "null" <> " AS header",
      bodyF <> " AS body"
    ] <>
    " FROM ( "<>selectQuery<>") t"
  ) (V.singleton . B.encodeValue . JSON.Array . V.map JSON.Object $ rows) True
  where
    bodyF
      | asCsv = asCsvF
      | isSingle = asJsonSingleF
      | otherwise = asJsonF

addRelations :: [Relation] -> Maybe ReadRequest -> ReadRequest -> Either Text ReadRequest
addRelations allRelations parentNode node@(Node readNode@(query, (name, _)) forest) =
  case parentNode of
    (Just (Node (Select{from=[parentTable]}, (_, _)) _)) -> Node <$> (addRel readNode <$> rel) <*> updatedForest
      where
        rel = note ("no relation between " <> parentTable <> " and " <> name)
            $  findRelationByTable name parentTable
           <|> findRelationByTable parentTable name
           <|> findRelationByColumn parentTable name
        addRel :: (ReadQuery, (NodeName, Maybe Relation)) -> Relation -> (ReadQuery, (NodeName, Maybe Relation))
        addRel (q, (n, _)) r = (q {from=fromRelation}, (n, Just r))
          where fromRelation = map (\t -> if t == n then tableName (relTable r) else t) (from q)

    _ -> Node (query, (name, Nothing)) <$> updatedForest
  where
    updatedForest = mapM (addRelations allRelations (Just node)) forest
    -- Searches through all the relations and returns a match given the parameter conditions.
    -- Will only find a relation where both schemas are in the PostgREST schema.
    -- `findRelationByColumn` also does a ducktype check to see if the column name has any variation of `id` or `fk`. If so then the relation is returned as a match.
    findRelationByTable t1 t2 =
      find (\r -> t1 == tableName (relTable r) && t2 == tableName (relFTable r)) allRelations
    findRelationByColumn t c =
      find (\r -> t == tableName (relFTable r) && length (relFColumns r) == 1 && c `colMatches` (colName . head . relFColumns) r) allRelations
      where n `colMatches` rc = (cs ("^" <> rc <> "_?(?:|[iI][dD]|[fF][kK])$") :: BS.ByteString) =~ (cs n :: BS.ByteString)

addJoinConditions :: ReadRequest -> Either Text ReadRequest
addJoinConditions (Node (query, (n, r)) forest) =
  case r of
    Nothing -> Node (updatedQuery, (n,r))  <$> updatedForest -- this is the root node
    Just rel@(Relation{relType=Child}) -> Node (addCond updatedQuery (getJoinConditions rel),(n,r)) <$> updatedForest
    Just (Relation{relType=Parent}) -> Node (updatedQuery, (n,r)) <$> updatedForest
    Just rel@(Relation{relType=Many, relLTable=(Just linkTable)}) ->
      Node (qq, (n, r)) <$> updatedForest
      where
         q = addCond updatedQuery (getJoinConditions rel)
         qq = q{from=tableName linkTable : from q}
    _ -> Left "unknown relation"
  where
    -- add parentTable and parentJoinConditions to the query
    updatedQuery = foldr (flip addCond) query parentJoinConditions
      where
        parentJoinConditions = map (getJoinConditions . snd) parents
        parents = mapMaybe (getParents . rootLabel) forest
        getParents (_, (tbl, Just rel@(Relation{relType=Parent}))) = Just (tbl, rel)
        getParents _ = Nothing
    updatedForest = mapM addJoinConditions forest
    addCond q con = q{flt_=con ++ flt_ q}

asJson :: StatementT
asJson s = s {
  B.stmtTemplate =
    "array_to_json(coalesce(array_agg(row_to_json(t)), '{}'))::character varying from ("
    <> B.stmtTemplate s <> ") t" }

callProc :: Text -> JSON.Object -> PStmt
callProc proc params = do
  let args = intercalate "," $ map assignment (HM.toList params)
  B.Stmt ("select * from " <> proc <> "(" <> args <> ")") empty True
  where
    assignment (n,v) = pgFmtIdent n <> ":=" <> insertableValue v

operators :: [(Text, SqlFragment)]
operators = [
  ("eq", "="),
  ("gte", ">="), -- has to be before gt (parsers)
  ("gt", ">"),
  ("lte", "<="), -- has to be before lt (parsers)
  ("lt", "<"),
  ("neq", "<>"),
  ("like", "like"),
  ("ilike", "ilike"),
  ("in", "in"),
  ("notin", "not in"),
  ("isnot", "is not"), -- has to be before is (parsers)
  ("is", "is"),
  ("@@", "@@"),
  ("@>", "@>"),
  ("<@", "<@")
  ]

pgFmtIdent :: SqlFragment -> SqlFragment
pgFmtIdent x = "\"" <> replace "\"" "\"\"" (trimNullChars $ cs x) <> "\""

pgFmtLit :: SqlFragment -> SqlFragment
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> replace "'" "''" trimmed <> "'"
     slashed = replace "\\" "\\\\" escaped in
 if "\\\\" `isInfixOf` escaped
   then "E" <> slashed
   else slashed

requestToCountQuery :: DbRequest -> SqlQuery
requestToCountQuery (DbMutate _) = undefined
requestToCountQuery (DbRead (Node (Select _ _ conditions _, (mainTbl, _)) _)) =
 unwords [
   "SELECT pg_catalog.count(1)",
   "FROM ", mainTbl,
   ("WHERE " <> intercalate " AND " ( map (pgFmtCondition mainTbl) localConditions )) `emptyOnNull` localConditions
   ]
 where
   fn  (Filter{value=VText _}) = True
   fn  (Filter{value=VForeignKey _ _}) = False
   localConditions = filter fn conditions

requestToQuery :: DbRequest -> SqlQuery
requestToQuery (DbMutate (Insert _ (PayloadParseError _))) = undefined
requestToQuery (DbMutate (Update _ (PayloadParseError _) _)) = undefined
requestToQuery (DbRead (Node (Select colSelects tbls conditions ord, (nodeName, maybeRelation)) forest)) =
  query
  where
    -- TODO! the folloing helper functions are just to remove the "schema" part when the table is "source" which is the name
    -- of our WITH query part
    mainTbl = fromMaybe nodeName (tableName . relTable <$> maybeRelation)
    -- tblSchema tbl = if tbl == sourceCTEName then "" else schema
    -- qi = QualifiedIdentifier (tblSchema mainTbl) mainTbl
    -- toQi t = QualifiedIdentifier (tblSchema t) t
    query = unwords [
      "SELECT ", intercalate ", " (map (pgFmtSelectItem mainTbl) colSelects ++ selects),
      "FROM ", intercalate ", " tbls,
      unwords (map joinStr joins),
      ("WHERE " <> intercalate " AND " ( map (pgFmtCondition mainTbl) localConditions )) `emptyOnNull` localConditions,
      orderF (fromMaybe [] ord)
      ]
    orderF ts =
        if null ts
            then ""
            else "ORDER BY " <> clause
        where
            clause = intercalate "," (map queryTerm ts)
            queryTerm :: OrderTerm -> Text
            queryTerm t = " "
                <> cs (pgFmtColumn mainTbl $ otTerm t) <> " "
                <> (cs.show) (otDirection t) <> " "
                <> maybe "" (cs.show) (otNullOrder t) <> " "
    (joins, selects) = foldr getQueryParts ([],[]) forest
    parentTables = map snd joins
    parentConditions = join $ map (( `filter` conditions ) . filterParentConditions) parentTables
    localConditions = conditions \\ parentConditions
    joinStr :: (SqlFragment, TableName) -> SqlFragment
    joinStr (sql, t) = "LEFT OUTER JOIN " <> sql <> " ON " <>
      intercalate " AND " ( map (pgFmtCondition mainTbl) joinConditions )
      where
        joinConditions = filter (filterParentConditions t) conditions
    filterParentConditions parentTable (Filter _ _ (VForeignKey t _)) =
      parentTable == t
    filterParentConditions _ _ = False
    getQueryParts :: Tree ReadNode -> ([(SqlFragment, TableName)], [SqlFragment]) -> ([(SqlFragment,TableName)], [SqlFragment])
    getQueryParts (Node n@(_, (name, Just (Relation {relType=Child,relTable=Table{tableName=table}}))) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE(("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> "), '[]') AS " <> pgFmtIdent name
           where subquery = requestToQuery (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just (Relation {relType=Parent,relTable=Table{tableName=table}}))) forst) (j,s) = (joi:j,sel:s)
      where
        sel = "row_to_json(" <> table <> ".*) AS "<>pgFmtIdent name --TODO must be singular
        joi = ("( " <> subquery <> " ) AS " <> table, table)
          where subquery = requestToQuery (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just (Relation {relType=Many,relTable=Table{tableName=table}}))) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE (("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> "), '[]') AS " <> pgFmtIdent name
           where subquery = requestToQuery (DbRead (Node n forst))
    --the following is just to remove the warning
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts (Node (_,(_,Nothing)) _) _ = undefined
requestToQuery (DbMutate (Insert mainTbl (PayloadJSON (UniformObjects rows)))) =
  let cols = map pgFmtIdent $ fromMaybe [] (HM.keys <$> (rows V.!? 0))
      colsString = intercalate ", " cols in
  unwords [
    "INSERT INTO ", mainTbl,
    " (" <> colsString <> ")" <>
    " SELECT " <> colsString <>
    " FROM json_populate_recordset(null::" , mainTbl, ", ?)"
    ]
requestToQuery (DbMutate (Update mainTbl (PayloadJSON (UniformObjects rows)) conditions)) =
  case rows V.!? 0 of
    Just obj ->
      let assignments = map
            (\(k,v) -> pgFmtIdent k <> "=" <> insertableValue v) $ HM.toList obj in
      unwords [
        "UPDATE ", mainTbl,
        " SET " <> intercalate "," assignments <> " ",
        ("WHERE " <> intercalate " AND " ( map (pgFmtCondition mainTbl ) conditions )) `emptyOnNull` conditions
        ]
    Nothing -> undefined

requestToQuery (DbMutate (Delete mainTbl conditions)) =
  query
  where
    query = unwords [
      "DELETE FROM ", mainTbl,
      ("WHERE " <> intercalate " AND " ( map (pgFmtCondition mainTbl ) conditions )) `emptyOnNull` conditions
      ]

sourceCTEName :: SqlFragment
sourceCTEName = "pg_source"

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  cs $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = cs . show $ b
unquoted v = cs $ JSON.encode v

-- private functions
asCsvF :: SqlFragment
asCsvF = asCsvHeaderF <> " || '\n' || " <> asCsvBodyF
  where
    asCsvHeaderF =
      "(SELECT string_agg(a.k, ',')" <>
      "  FROM (" <>
      "    SELECT json_object_keys(r)::TEXT as k" <>
      "    FROM ( " <>
      "      SELECT row_to_json(hh) as r from " <> sourceCTEName <> " as hh limit 1" <>
      "    ) s" <>
      "  ) a" <>
      ")"
    asCsvBodyF = "coalesce(string_agg(substring(t::text, 2, length(t::text) - 2), '\n'), '')"

asJsonF :: SqlFragment
asJsonF = "array_to_json(array_agg(row_to_json(t)))::character varying"

asJsonSingleF :: SqlFragment --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF = "string_agg(row_to_json(t)::text, ',')::character varying "

locationF :: [Text] -> SqlFragment
locationF pKeys =
    "(" <>
    " WITH s AS (SELECT row_to_json(ss) as r from " <> sourceCTEName <> " as ss  limit 1)" <>
    " SELECT string_agg(json_data.key || '=' || coalesce( 'eq.' || json_data.value, 'is.null'), '&')" <>
    " FROM s, json_each_text(s.r) AS json_data" <>
    (
      if null pKeys
      then ""
      else " WHERE json_data.key IN ('" <> intercalate "','" pKeys <> "')"
    ) <>
    ")"

limitF :: NonnegRange -> SqlFragment
limitF r  = "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit  = maybe "ALL" (cs . show) $ rangeLimit r
    offset = cs . show $ rangeOffset r

getJoinConditions :: Relation -> [Filter]
getJoinConditions (Relation t cols ft fcs typ lt lc1 lc2) =
  case typ of
    Child  -> zipWith (toFilter tN ftN) cols fcs
    Parent -> zipWith (toFilter tN ftN) cols fcs
    Many   -> zipWith (toFilter tN ltN) cols (fromMaybe [] lc1) ++ zipWith (toFilter ftN ltN) fcs (fromMaybe [] lc2)
  where
    tN = tableName t
    ftN = tableName ft
    ltN = fromMaybe "" (tableName <$> lt)
    toFilter :: Text -> Text -> Column -> Column -> Filter
    toFilter tb ftb c fc = Filter (colName c, Nothing) "=" (VForeignKey tb (ForeignKey fc{colTable=(colTable fc){tableName=ftb}}))

emptyOnNull :: Text -> [a] -> Text
emptyOnNull val x = if null x then "" else val

insertableValue :: JSON.Value -> SqlFragment
insertableValue JSON.Null = "null"
insertableValue v = (<> "::unknown") . pgFmtLit $ unquoted v

whiteList :: Text -> SqlFragment
whiteList val = fromMaybe
  (cs (pgFmtLit val) <> "::unknown ")
  (find ((==) . toLower $ val) ["null","true","false"])

pgFmtColumn :: TableName -> Text -> SqlFragment
pgFmtColumn table "*" = table <> ".*"
pgFmtColumn table c = table <> "." <> pgFmtIdent c

pgFmtField :: TableName -> Field -> SqlFragment
pgFmtField table (c, jp) = pgFmtColumn table c <> pgFmtJsonPath jp

pgFmtSelectItem :: TableName -> SelectItem -> SqlFragment
pgFmtSelectItem table (f@(_, jp), Nothing) = pgFmtField table f <> pgFmtAsJsonPath jp
pgFmtSelectItem table (f@(_, jp), Just cast ) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> pgFmtAsJsonPath jp

pgFmtCondition :: TableName -> Filter -> SqlFragment
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
      VForeignKey _ _ -> pgFmtColumn table col
    sqlValue = valToStr val
    getInner v = case v of
      VText s -> s
      _      -> ""
    valToStr v = case v of
      VText s -> pgFmtValue opCode s
      VForeignKey _ (ForeignKey Column{colTable=Table{tableName=ft}, colName=fc}) -> pgFmtColumn ft fc
      _ -> ""

pgFmtValue :: Text -> Text -> SqlFragment
pgFmtValue opCode val =
 case opCode of
   "like" -> unknownLiteral $ T.map star val
   "ilike" -> unknownLiteral $ T.map star val
   "in" -> "(" <> intercalate ", " (map unknownLiteral $ split (==',') val) <> ") "
   "notin" -> "(" <> intercalate ", " (map unknownLiteral $ split (==',') val) <> ") "
   "@@" -> "to_tsquery(" <> unknownLiteral val <> ") "
   _    -> unknownLiteral val
 where
   star c = if c == '*' then '%' else c
   unknownLiteral = (<> "::unknown ") . pgFmtLit

pgFmtOperator :: Text -> SqlFragment
pgFmtOperator opCode = fromMaybe "=" $ M.lookup opCode operatorsMap
  where
    operatorsMap = M.fromList operators

pgFmtJsonPath :: Maybe JsonPath -> SqlFragment
pgFmtJsonPath (Just [x]) = "->>" <> pgFmtLit x
pgFmtJsonPath (Just (x:xs)) = "->" <> pgFmtLit x <> pgFmtJsonPath ( Just xs )
pgFmtJsonPath _ = ""

pgFmtAsJsonPath :: Maybe JsonPath -> SqlFragment
pgFmtAsJsonPath Nothing = ""
pgFmtAsJsonPath (Just xx) = " AS " <> last xx

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')
