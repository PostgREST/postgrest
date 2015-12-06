{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections #-}
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
  , sourceSubqueryName
  , unquoted
  ) where

import qualified Hasql                   as H
import qualified Hasql.Backend           as B
import qualified Hasql.Postgres          as P

import qualified Data.Aeson              as JSON

import           PostgREST.RangeQuery    (NonnegRange, rangeLimit, rangeOffset)
import           Control.Error           (note, fromMaybe, mapMaybe)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (find)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, intercalate, unwords, replace, isInfixOf, toLower, split)
import qualified Data.Text as T          (map, takeWhile)
import           Data.String.Conversions (cs)
import           Control.Applicative     (empty, (<|>))
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

type PStmt = H.Stmt P.Postgres
instance Monoid PStmt where
  mappend (B.Stmt query params prep) (B.Stmt query' params' prep') =
    B.Stmt (query <> query') (params <> params') (prep && prep')
  mempty = B.Stmt "" empty True
type StatementT = PStmt -> PStmt

createReadStatement :: SqlQuery -> NonnegRange -> Bool -> Bool -> Bool -> B.Stmt P.Postgres
createReadStatement selectQuery range isSingle countTable asCsv =
  B.Stmt (
    wrapLimitedQuery selectQuery [
      if countTable then countAllF else countNoneF,
      countF,
      "null", -- location header can not be calucalted
      if asCsv
        then asCsvF
        else if isSingle then asJsonSingleF else asJsonF
    ] selectStarF range
  ) V.empty True

createWriteStatement :: SqlQuery -> SqlQuery -> Bool -> Bool ->
                        [Text] -> Bool -> Payload -> B.Stmt P.Postgres
createWriteStatement _ _ _ _ _ _ (PayloadParseError _) = undefined
createWriteStatement selectQuery mutateQuery isSingle echoRequested
                     pKeys asCsv (PayloadJSON (UniformObjects rows)) =
  B.Stmt (
    wrapQuery mutateQuery [
      countNoneF, -- when updateing it does not make sense
      countF,
      if isSingle then locationF pKeys else "null",
      if echoRequested
      then
        if asCsv
        then asCsvF
        else if isSingle then asJsonSingleF else asJsonF
      else "null"

    ] selectQuery
  ) (V.singleton . B.encodeValue . JSON.Array . V.map JSON.Object $ rows) True

addRelations :: Schema -> [Relation] -> Maybe ReadRequest -> ReadRequest -> Either Text ReadRequest
addRelations schema allRelations parentNode node@(Node n@(query, (table, _)) forest) =
  case parentNode of
    Nothing -> Node (query, (table, Nothing)) <$> updatedForest
    (Just (Node (_, (parentTable, _)) _)) -> Node <$> (addRel n <$> rel) <*> updatedForest
      where
        rel = note ("no relation between " <> table <> " and " <> parentTable)
            $  findRelation schema table parentTable
           <|> findRelation schema parentTable table
        addRel :: (ReadQuery, (NodeName, Maybe Relation)) -> Relation -> (ReadQuery, (NodeName, Maybe Relation))
        addRel (q, (t, _)) r = (q, (t, Just r))
  where
    updatedForest = mapM (addRelations schema allRelations (Just node)) forest
    findRelation s t1 t2 =
      find (\r -> s == (tableSchema . relTable) r && t1 == (tableName . relTable) r && t2 == (tableName . relFTable) r) allRelations

addJoinConditions :: Schema -> ReadRequest -> Either Text ReadRequest
addJoinConditions schema (Node (query, (n, r)) forest) =
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
    updatedForest = mapM (addJoinConditions schema) forest
    addCond q con = q{flt_=con ++ flt_ q}

asJson :: StatementT
asJson s = s {
  B.stmtTemplate =
    "array_to_json(coalesce(array_agg(row_to_json(t)), '{}'))::character varying from ("
    <> B.stmtTemplate s <> ") t" }

callProc :: QualifiedIdentifier -> JSON.Object -> PStmt
callProc qi params = do
  let args = intercalate "," $ map assignment (HM.toList params)
  B.Stmt ("select * from " <> fromQi qi <> "(" <> args <> ")") empty True
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
pgFmtIdent x =
 let escaped = replace "\"" "\"\"" (trimNullChars $ cs x) in
 if (cs escaped :: BS.ByteString) =~ danger
   then "\"" <> escaped <> "\""
   else escaped
 where danger = "^$|^[^a-z_]|[^a-z_0-9]" :: BS.ByteString

pgFmtLit :: SqlFragment -> SqlFragment
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> replace "'" "''" trimmed <> "'"
     slashed = replace "\\" "\\\\" escaped in
 if "\\\\" `isInfixOf` escaped
   then "E" <> slashed
   else slashed

requestToQuery :: Schema -> DbRequest -> SqlQuery
requestToQuery _ (DbMutate (Insert _ (PayloadParseError _))) = undefined
requestToQuery _ (DbMutate (Update _ (PayloadParseError _) _)) = undefined
requestToQuery schema (DbRead (Node (Select colSelects tbls conditions ord, (mainTbl, _)) forest)) =
  query
  where
    -- TODO! the folloing helper functions are just to remove the "schema" part when the table is "source" which is the name
    -- of our WITH query part
    tblSchema tbl = if tbl == sourceSubqueryName then "" else schema
    qi = QualifiedIdentifier (tblSchema mainTbl) mainTbl
    toQi t = QualifiedIdentifier (tblSchema t) t
    query = unwords [
      ("WITH " <> intercalate ", " (map fst withs)) `emptyOnNull` withs,
      "SELECT ", intercalate ", " (map (pgFmtSelectItem qi) colSelects ++ selects),
      "FROM ", intercalate ", " (map (fromQi . toQi) tbls ++ map snd withs),
      ("WHERE " <> intercalate " AND " ( map (pgFmtCondition qi ) conditions )) `emptyOnNull` conditions,
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
                <> cs (pgFmtColumn qi $ otTerm t) <> " "
                <> (cs.show) (otDirection t) <> " "
                <> maybe "" (cs.show) (otNullOrder t) <> " "
    (withs, selects) = foldr getQueryParts ([],[]) forest
    getQueryParts :: Tree ReadNode -> ([(SqlFragment, Text)], [SqlFragment]) -> ([(SqlFragment,Text)], [SqlFragment])
    getQueryParts (Node n@(_, (table, Just (Relation {relType=Child}))) forst) (w,s) = (w,sel:s)
      where
        sel = "("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> ") AS " <> table
           where subquery = requestToQuery schema (DbRead (Node n forst))
    getQueryParts (Node n@(_, (table, Just (Relation {relType=Parent}))) forst) (w,s) = (wit:w,sel:s)
      where
        sel = "row_to_json(" <> table <> ".*) AS "<>table --TODO must be singular
        wit = (table <> " AS ( " <> subquery <> " )", table)
          where subquery = requestToQuery schema (DbRead (Node n forst))
    getQueryParts (Node n@(_, (table, Just (Relation {relType=Many}))) forst) (w,s) = (w,sel:s)
      where
        sel = "("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> ") AS " <> table
           where subquery = requestToQuery schema (DbRead (Node n forst))
    --the following is just to remove the warning
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts (Node (_,(_,Nothing)) _) _ = undefined
requestToQuery schema (DbMutate (Insert mainTbl (PayloadJSON (UniformObjects rows)))) =
  let qi = QualifiedIdentifier schema mainTbl
      cols = map pgFmtIdent $ fromMaybe [] (HM.keys <$> (rows V.!? 0))
      colsString = intercalate ", " cols in
  unwords [
    "INSERT INTO ", fromQi qi,
    " (" <> colsString <> ")" <>
    " SELECT " <> colsString <>
    " FROM json_populate_recordset(null::" , fromQi qi, ", ?)",
    " RETURNING " <> fromQi qi <> ".*"
    ]
requestToQuery schema (DbMutate (Update mainTbl (PayloadJSON (UniformObjects rows)) conditions)) =
  case rows V.!? 0 of
    Just obj ->
      let assignments = map
            (\(k,v) -> pgFmtIdent k <> "=" <> insertableValue v) $ HM.toList obj in
      unwords [
        "UPDATE ", fromQi qi,
        " SET " <> intercalate "," assignments <> " ",
        ("WHERE " <> intercalate " AND " ( map (pgFmtCondition qi ) conditions )) `emptyOnNull` conditions,
        "RETURNING " <> fromQi qi <> ".*"
        ]
    Nothing -> undefined
  where
    qi = QualifiedIdentifier schema mainTbl

requestToQuery schema (DbMutate (Delete mainTbl conditions)) =
  query
  where
    qi = QualifiedIdentifier schema mainTbl
    query = unwords [
      "DELETE FROM ", fromQi qi,
      ("WHERE " <> intercalate " AND " ( map (pgFmtCondition qi ) conditions )) `emptyOnNull` conditions,
      "RETURNING " <> fromQi qi <> ".*"
      ]

sourceSubqueryName :: SqlFragment
sourceSubqueryName = "pg_source"

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
      "      SELECT row_to_json(hh) as r from " <> sourceSubqueryName <> " as hh limit 1" <>
      "    ) s" <>
      "  ) a" <>
      ")"
    asCsvBodyF = "coalesce(string_agg(substring(t::text, 2, length(t::text) - 2), '\n'), '')"

asJsonF :: SqlFragment
asJsonF = "array_to_json(array_agg(row_to_json(t)))::character varying"

asJsonSingleF :: SqlFragment --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF = "string_agg(row_to_json(t)::text, ',')::character varying "

countAllF :: SqlFragment
countAllF = "(SELECT pg_catalog.count(1) FROM (SELECT * FROM " <> sourceSubqueryName <> ") a )"

countF :: SqlFragment
countF = "pg_catalog.count(t)"

countNoneF :: SqlFragment
countNoneF = "null"

locationF :: [Text] -> SqlFragment
locationF pKeys =
    "(" <>
    " WITH s AS (SELECT row_to_json(ss) as r from " <> sourceSubqueryName <> " as ss  limit 1)" <>
    " SELECT string_agg(json_data.key || '=' || coalesce( 'eq.' || json_data.value, 'is.null'), '&')" <>
    " FROM s, json_each_text(s.r) AS json_data" <>
    (
      if null pKeys
      then ""
      else " WHERE json_data.key IN ('" <> intercalate "','" pKeys <> "')"
    ) <>
    ")"

fromQi :: QualifiedIdentifier -> SqlFragment
fromQi t = (if s == "" then "" else pgFmtIdent s <> ".") <> pgFmtIdent n
  where
    n = qiName t
    s = qiSchema t

getJoinConditions :: Relation -> [Filter]
getJoinConditions (Relation t cols ft fcs typ lt lc1 lc2) =
  case typ of
    Child  -> zipWith (toFilter tN ftN) cols fcs
    Parent -> zipWith (toFilter tN ftN) cols fcs
    Many   -> zipWith (toFilter tN ltN) cols (fromMaybe [] lc1) ++ zipWith (toFilter ftN ltN) fcs (fromMaybe [] lc2)
  where
    s = if typ == Parent then "" else tableSchema t
    tN = tableName t
    ftN = tableName ft
    ltN = fromMaybe "" (tableName <$> lt)
    toFilter :: Text -> Text -> Column -> Column -> Filter
    toFilter tb ftb c fc = Filter (colName c, Nothing) "=" (VForeignKey (QualifiedIdentifier s tb) (ForeignKey fc{colTable=(colTable fc){tableName=ftb}}))

emptyOnNull :: Text -> [a] -> Text
emptyOnNull val x = if null x then "" else val

insertableValue :: JSON.Value -> SqlFragment
insertableValue JSON.Null = "null"
insertableValue v = (<> "::unknown") . pgFmtLit $ unquoted v

whiteList :: Text -> SqlFragment
whiteList val = fromMaybe
  (cs (pgFmtLit val) <> "::unknown ")
  (find ((==) . toLower $ val) ["null","true","false"])

pgFmtColumn :: QualifiedIdentifier -> Text -> SqlFragment
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c = fromQi table <> "." <> pgFmtIdent c

pgFmtField :: QualifiedIdentifier -> Field -> SqlFragment
pgFmtField table (c, jp) = pgFmtColumn table c <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> SqlFragment
pgFmtSelectItem table (f@(_, jp), Nothing) = pgFmtField table f <> pgFmtAsJsonPath jp
pgFmtSelectItem table (f@(_, jp), Just cast ) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> pgFmtAsJsonPath jp

pgFmtCondition :: QualifiedIdentifier -> Filter -> SqlFragment
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
      VForeignKey (QualifiedIdentifier s _) (ForeignKey Column{colTable=Table{tableName=ft}, colName=fc}) -> pgFmtColumn qi fc
        where qi = QualifiedIdentifier (if ft == sourceSubqueryName then "" else s) ft
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

withSourceF :: SqlFragment -> SqlFragment
withSourceF s = "WITH " <> sourceSubqueryName <> " AS (" <> s <>")"

fromF :: SqlFragment -> SqlFragment -> SqlFragment
fromF sel limit = "FROM (" <> sel <> " " <> limit <> ") t"

limitF :: NonnegRange -> SqlFragment
limitF r  = "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit  = maybe "ALL" (cs . show) $ rangeLimit r
    offset = cs . show $ rangeOffset r

selectStarF :: SqlFragment
selectStarF = "SELECT * FROM " <> sourceSubqueryName

wrapLimitedQuery :: SqlQuery -> [Text] -> Text -> NonnegRange -> SqlQuery
wrapLimitedQuery source selectColumns returnSelect range =
  withSourceF source <>
  " SELECT " <>
  intercalate ", " selectColumns <>
  " " <>
  fromF returnSelect ( limitF range )

wrapQuery :: SqlQuery -> [Text] -> Text -> SqlQuery
wrapQuery source selectColumns returnSelect =
  withSourceF source <>
  " SELECT " <>
  intercalate ", " selectColumns <>
  " " <>
  fromF returnSelect ""
