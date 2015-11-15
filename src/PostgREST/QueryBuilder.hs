{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PostgREST.QueryBuilder (
    addRelations
  , addJoinConditions
  , asCsvF
  , asJson
  , asJsonF
  , asJsonSingleF
  , callProc
  , countAllF
  , countF
  , countNoneF
  , locationF
  , operators
  , pgFmtIdent
  , pgFmtLit
  , requestToQuery
  , selectStarF
  , sourceSubqueryName
  , unquoted
  , wrapQuery
  ) where

import qualified Hasql                   as H
import qualified Hasql.Backend           as B
import qualified Hasql.Postgres          as P

import qualified Data.Aeson              as JSON

import           Control.Error           (note, fromMaybe, mapMaybe)
import           Control.Monad           (join)
import           Data.List               (find)
import           Data.Monoid             ((<>))
import           Data.Maybe              (fromJust)
import           Data.Text               (Text, intercalate, unwords, replace, isInfixOf, toLower, split)
import qualified Data.Text as T          (map, takeWhile)
import           Data.String.Conversions (cs)
import qualified Data.ByteString.Char8   as BS
import qualified Data.Map                as M
import qualified Data.HashMap.Strict     as H
import           Data.Tree               (Tree(..))
import           Data.Scientific         (FPFormat (..), formatScientific, isInteger)
import           Text.Regex.TDFA         ((=~))
import           Control.Applicative     (empty, (<|>))

import           PostgREST.RangeQuery    (NonnegRange, rangeLimit, rangeOffset)
import           PostgREST.Types

import           Prelude hiding          (unwords)

type PStmt = H.Stmt P.Postgres

instance Monoid PStmt where
  mappend (B.Stmt query params prep) (B.Stmt query' params' prep') =
    B.Stmt (query <> query') (params <> params') (prep && prep')
  mempty = B.Stmt "" empty True

type StatementT = PStmt -> PStmt

addRelations :: Schema -> [Relation] -> Maybe ApiRequest -> ApiRequest -> Either Text ApiRequest
addRelations schema allRelations parentNode node@(Node n@(query, (table, _)) forest) =
  case parentNode of
    Nothing -> Node (query, (table, Nothing)) <$> updatedForest
    (Just (Node (_, (parentTable, _)) _)) -> Node <$> (addRel n <$> rel) <*> updatedForest
      where
        rel = note ("no relation between " <> table <> " and " <> parentTable)
            $  findRelation schema table parentTable
           <|> findRelation schema parentTable table
        addRel :: (Query, (NodeName, Maybe Relation)) -> Relation -> (Query, (NodeName, Maybe Relation))
        addRel (q, (t, _)) r = (q, (t, Just r))
  where
    updatedForest = mapM (addRelations schema allRelations (Just node)) forest
    findRelation s t1 t2 =
      find (\r -> s == (tableSchema . relTable) r && t1 == (tableName . relTable) r && t2 == (tableName . relFTable) r) allRelations

addJoinConditions :: Schema -> ApiRequest -> Either Text ApiRequest
addJoinConditions schema (Node (query, (n, r)) forest) =
  case r of
    Nothing -> Node (updatedQuery, (n,r))  <$> updatedForest -- this is the root node
    Just rel@(Relation{relType=Child}) -> Node (addCond updatedQuery (getJoinConditions rel),(n,r)) <$> updatedForest
    Just (Relation{relType=Parent}) -> Node (updatedQuery, (n,r)) <$> updatedForest
    Just rel@(Relation{relType=Many, relLTable=(Just linkTable)}) ->
      Node (qq, (n, r)) <$> updatedForest
      where
         q = addCond updatedQuery (getJoinConditions rel)
         qq = q{from=tableQi linkTable : from q}
    _ -> Left "unknown relation"
  where
    -- add parentTable and parentJoinConditions to the query
    updatedQuery = foldr (flip addCond) (query{from = parentTables ++ from query}) parentJoinConditions
      where
        parentJoinConditions = map (getJoinConditions . snd) parents
        parentTables = map fst parents
        parents = mapMaybe (getParents . rootLabel) forest
        getParents (_, (_, Just rel@(Relation{relType=Parent,relTable=tbl}))) = Just (tableQi tbl, rel)
        getParents _ = Nothing
    updatedForest = mapM (addJoinConditions schema) forest
    addCond q con = q{where_=con ++ where_ q}

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

asJson :: StatementT
asJson s = s {
  B.stmtTemplate =
    "array_to_json(coalesce(array_agg(row_to_json(t)), '{}'))::character varying from ("
    <> B.stmtTemplate s <> ") t" }

asJsonF :: SqlFragment
asJsonF = "array_to_json(array_agg(row_to_json(t)))::character varying"

asJsonSingleF :: SqlFragment --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF = "string_agg(row_to_json(t)::text, ',')::character varying "

callProc :: QualifiedIdentifier -> JSON.Object -> PStmt
callProc qi params = do
  let args = intercalate "," $ map assignment (H.toList params)
  B.Stmt ("select * from " <> fromQi qi <> "(" <> args <> ")") empty True
  where
    assignment (n,v) = pgFmtIdent n <> ":=" <> insertableValue v

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

requestToQuery :: Schema -> ApiRequest -> SqlQuery
requestToQuery schema (Node (Select colSelects tbls conditions ord, _) forest) =
  query
  where
    query = unwords [
      ("WITH " <> intercalate ", " withs) `emptyOnNull` withs,
      "SELECT ", intercalate ", " (map pgFmtSelectItem colSelects ++ selects),
      "FROM ", intercalate ", " (map fromQi tbls),
      ("WHERE " <> intercalate " AND " (map pgFmtCondition conditions)) `emptyOnNull` conditions,
      orderF (fromMaybe [] ord)
      ]
    (withs, selects) = foldr getQueryParts ([],[]) forest
    getQueryParts :: Tree ApiNode -> ([SqlFragment], [SqlFragment]) -> ([SqlFragment], [SqlFragment])
    getQueryParts (Node n@(_, (table, Just (Relation {relType=Child}))) forst) (w,s) = (w,sel:s)
      where
        sel = "("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> ") AS " <> table
           where subquery = requestToQuery schema (Node n forst)
    getQueryParts (Node n@(_, (table, Just (Relation {relType=Parent}))) forst) (w,s) = (wit:w,sel:s)
      where
        sel = "row_to_json(" <> table <> ".*) AS "<>table --TODO must be singular
        wit = table <> " AS ( " <> subquery <> " )"
          where subquery = requestToQuery schema (Node n forst)
    getQueryParts (Node n@(_, (table, Just (Relation {relType=Many}))) forst) (w,s) = (w,sel:s)
      where
        sel = "("
           <> "SELECT array_to_json(array_agg(row_to_json("<>table<>"))) "
           <> "FROM (" <> subquery <> ") " <> table
           <> ") AS " <> table
           where subquery = requestToQuery schema (Node n forst)
    --the following is just to remove the warning
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts (Node (_,(_,Nothing)) _) _ = undefined

requestToQuery schema (Node (Insert _ flds vals, (mainTbl, _)) _) =
  query
  where
    qi = [schema,mainTbl]
    query = unwords [
      "INSERT INTO ", fromQi qi,
      " (" <> intercalate ", " (map (pgFmtIdent . last . fst) flds) <> ") ",
      "VALUES " <> intercalate ", "
        ( map (\v ->
            "(" <>
            intercalate ", " ( map insertableValue v ) <>
            ")"
          ) vals
        ),
      "RETURNING " <> fromQi qi <> ".*"
      ]

requestToQuery schema (Node (Update _ setWith conditions, (mainTbl, _)) _) =
  query
  where
    qi = [schema,mainTbl]
    query = unwords [
      "UPDATE ", fromQi qi,
      " SET " <> intercalate ", " (map formatSet (M.toList setWith)) <> " ",
      ("WHERE " <> intercalate " AND " (map pgFmtCondition conditions)) `emptyOnNull` conditions,
      "RETURNING " <> fromQi qi <> ".*"
      ]
    formatSet ((qqi, jp), v) = fromQi qqi <> pgFmtJsonPath jp <> " = " <> insertableValue v

requestToQuery schema (Node (Delete _ conditions, (mainTbl, _)) _) =
  query
  where
    qi = [schema,mainTbl]
    query = unwords [
      "DELETE FROM ", fromQi qi,
      ("WHERE " <> intercalate " AND " (map pgFmtCondition conditions)) `emptyOnNull` conditions,
      "RETURNING " <> fromQi qi <> ".*"
      ]

selectStarF :: SqlFragment
selectStarF = "SELECT * FROM " <> sourceSubqueryName

sourceSubqueryName :: SqlFragment
sourceSubqueryName = "pg_source"

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  cs $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = cs . show $ b
unquoted v = cs $ JSON.encode v

wrapQuery :: SqlQuery -> [Text] -> Text ->  Maybe NonnegRange -> SqlQuery
wrapQuery source selectColumns returnSelect range =
  withSourceF source <>
  " SELECT " <>
  intercalate ", " selectColumns <>
  " " <>
  fromF returnSelect ( limitF range )

-- private functions
fromQi :: QualifiedIdentifier -> SqlFragment
fromQi t = intercalate "." $ map pgFmtIdent t

getJoinConditions :: Relation -> [Filter]
getJoinConditions (Relation t cols ft fcols typ mlt lc1 lc2) =
  case typ of
    Child  -> zipWith (toFilter t ft) cols fcols
    Parent -> zipWith (toFilter t ft) cols fcols
    Many   -> zipWith (toFilter t lt) cols (fromMaybe [] lc1) ++ zipWith (toFilter ft lt) fcols (fromMaybe [] lc2)
  where
    lt = fromJust mlt -- This is ok because it is only used when the relation is of the `Many` type
    toFilter tb ftb c fc = Filter (makeQi tb c, Nothing) "=" $ VForeignKey (makeQi ftb fc)
    makeQi tb c = (if tableName tb == sourceSubqueryName then [] else [tableSchema tb]) ++ [tableName tb, colName c]

emptyOnNull :: Text -> [a] -> Text
emptyOnNull val x = if null x then "" else val

orderF :: [OrderTerm] -> SqlFragment
orderF ts =
  if null ts
    then ""
    else "ORDER BY " <> clause
  where
    clause = intercalate "," (map queryTerm ts)
    queryTerm :: OrderTerm -> Text
    queryTerm t = " "
           <> cs (pgFmtIdent $ otTerm t) <> " "
           <> cs (otDirection t)         <> " "
           <> maybe "" cs (otNullOrder t) <> " "

insertableValue :: JSON.Value -> SqlFragment
insertableValue JSON.Null = "null"
insertableValue v = (<> "::unknown") . pgFmtLit $ unquoted v

whiteList :: Text -> SqlFragment
whiteList val = fromMaybe
  (cs (pgFmtLit val) <> "::unknown ")
  (find ((==) . toLower $ val) ["null","true","false"])

pgFmtColumn :: QualifiedIdentifier -> SqlFragment
pgFmtColumn qi =
  if col == "*"
    then fromQi table <> ".*"
    else fromQi qi
  where
    table = init qi
    col = last qi

pgFmtField :: Field -> SqlFragment
pgFmtField (c, jp) = pgFmtColumn c <> pgFmtJsonPath jp

pgFmtSelectItem :: SelectItem -> SqlFragment
pgFmtSelectItem (f@(_, jp), Nothing) = pgFmtField f <> pgFmtAsJsonPath jp
pgFmtSelectItem (f@(_, jp), Just cast) = "CAST (" <> pgFmtField f <> " AS " <> cast <> " )" <> pgFmtAsJsonPath jp

pgFmtCondition :: Filter -> SqlFragment
pgFmtCondition (Filter (col,jp) ops val) =
  notOp <> " " <> sqlCol  <> " " <> pgFmtOperator opCode <> " " <>
    if opCode `elem` ["is","isnot"] then whiteList innerVal else sqlValue
  where
    headPredicate:rest = split (=='.') ops
    hasNot caseTrue caseFalse = if headPredicate == "not" then caseTrue else caseFalse
    opCode      = hasNot (head rest) headPredicate
    notOp       = hasNot headPredicate ""
    sqlCol = case val of
      VText _ -> pgFmtColumn col <> pgFmtJsonPath jp
      VForeignKey _ -> pgFmtColumn col
    innerVal = case val of
      VText s -> s
      _ -> ""
    sqlValue = case val of
      VText s -> pgFmtValue opCode s
      VForeignKey qi -> pgFmtColumn qi

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

limitF :: Maybe NonnegRange -> SqlFragment
limitF r  = "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit  = maybe "ALL" (cs . show) $ join $ rangeLimit <$> r
    offset = cs . show $ fromMaybe 0 $ rangeOffset <$> r
