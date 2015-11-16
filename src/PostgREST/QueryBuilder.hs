{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections #-}
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

import           PostgREST.RangeQuery    (NonnegRange, rangeLimit, rangeOffset)
import           Control.Error           (note, fromMaybe, mapMaybe)
import           Control.Monad           (join)
import           Data.List               (find)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text as T
import           Data.String.Conversions (cs)
import qualified Data.HashMap.Strict     as H
import           Control.Applicative     (empty, (<|>))
import           Data.Tree               (Tree(..))
import           PostgREST.Types
import qualified Data.Map as M
import           Text.Regex.TDFA         ((=~))
import qualified Data.ByteString.Char8   as BS
import           Data.Scientific         ( FPFormat (..)
                                         , formatScientific
                                         , isInteger
                                         )

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
         qq = q{from=tableName linkTable : from q}
    _ -> Left "unknown relation"
  where
    -- add parentTable and parentJoinConditions to the query
    updatedQuery = foldr (flip addCond) (query{from = parentTables ++ from query}) parentJoinConditions
      where
        parentJoinConditions = map (getJoinConditions . snd) parents
        parentTables = map fst parents
        parents = mapMaybe (getParents . rootLabel) forest
        getParents (_, (tbl, Just rel@(Relation{relType=Parent}))) = Just (tbl, rel)
        getParents _ = Nothing
    updatedForest = mapM (addJoinConditions schema) forest
    addCond q con = q{where_=con ++ where_ q}

asCsvF :: T.Text
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

asJsonF :: T.Text
asJsonF = "array_to_json(array_agg(row_to_json(t)))::character varying"

asJsonSingleF :: T.Text --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF = "string_agg(row_to_json(t)::text, ',')::character varying "

callProc :: QualifiedIdentifier -> JSON.Object -> PStmt
callProc qi params = do
  let args = T.intercalate "," $ map assignment (H.toList params)
  B.Stmt ("select * from " <> fromQi qi <> "(" <> args <> ")") empty True
  where
    assignment (n,v) = pgFmtIdent n <> ":=" <> insertableValue v

countAllF :: T.Text
countAllF = "(SELECT pg_catalog.count(1) FROM (SELECT * FROM " <> sourceSubqueryName <> ") a )"

countF :: T.Text
countF = "pg_catalog.count(t)"

countNoneF :: T.Text
countNoneF = "null"

locationF :: [T.Text] -> T.Text
locationF pKeys =
    "(" <>
    " WITH s AS (SELECT row_to_json(ss) as r from " <> sourceSubqueryName <> " as ss  limit 1)" <>
    " SELECT string_agg(json_data.key || '=' || coalesce( 'eq.' || json_data.value, 'is.null'), '&')" <>
    " FROM s, json_each_text(s.r) AS json_data" <>
    (
      if null pKeys
      then ""
      else " WHERE json_data.key IN ('" <> T.intercalate "','" pKeys <> "')"
    ) <>
    ")"

operators :: [(T.Text, T.Text)]
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

pgFmtIdent :: T.Text -> T.Text
pgFmtIdent x =
 let escaped = T.replace "\"" "\"\"" (trimNullChars $ cs x) in
 if (cs escaped :: BS.ByteString) =~ danger
   then "\"" <> escaped <> "\""
   else escaped
 where danger = "^$|^[^a-z_]|[^a-z_0-9]" :: BS.ByteString

pgFmtLit :: T.Text -> T.Text
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> T.replace "'" "''" trimmed <> "'"
     slashed = T.replace "\\" "\\\\" escaped in
 if T.isInfixOf "\\\\" escaped
   then "E" <> slashed
   else slashed

requestToQuery :: Schema -> ApiRequest -> Text
requestToQuery schema (Node (Select colSelects tbls conditions ord, (mainTbl, _)) forest) =
  query
  where
    -- TODO! the folloing helper functions are just to remove the "schema" part when the table is "source" which is the name
    -- of our WITH query part
    tblSchema tbl = if tbl == sourceSubqueryName then "" else schema
    qi = QualifiedIdentifier (tblSchema mainTbl) mainTbl
    toQi t = QualifiedIdentifier (tblSchema t) t
    query = T.unwords [
      ("WITH " <> T.intercalate ", " withs) `emptyOnNull` withs,
      "SELECT ", T.intercalate ", " (map (pgFmtSelectItem qi) colSelects ++ selects),
      "FROM ", T.intercalate ", " (map (fromQi . toQi) tbls),
      ("WHERE " <> T.intercalate " AND " ( map (pgFmtCondition qi ) conditions )) `emptyOnNull` conditions,
      orderF (fromMaybe [] ord)
      ]
    (withs, selects) = foldr getQueryParts ([],[]) forest
    getQueryParts :: Tree ApiNode -> ([Text], [Text]) -> ([Text], [Text])
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
    qi = QualifiedIdentifier schema mainTbl
    query = T.unwords [
      "INSERT INTO ", fromQi qi,
      " (" <> T.intercalate ", " (map (pgFmtIdent . fst) flds) <> ") ",
      "VALUES " <> T.intercalate ", "
        ( map (\v ->
            "(" <>
            T.intercalate ", " ( map insertableValue v ) <>
            ")"
          ) vals
        ),
      "RETURNING " <> fromQi qi <> ".*"
      ]

requestToQuery schema (Node (Update _ setWith conditions, (mainTbl, _)) _) =
  query
  where
    qi = QualifiedIdentifier schema mainTbl
    query = T.unwords [
      "UPDATE ", fromQi qi,
      " SET " <> T.intercalate ", " (map formatSet (M.toList setWith)) <> " ",
      ("WHERE " <> T.intercalate " AND " ( map (pgFmtCondition qi ) conditions )) `emptyOnNull` conditions,
      "RETURNING " <> fromQi qi <> ".*"
      ]
    formatSet ((c, jp), v) = pgFmtIdent c <> pgFmtJsonPath jp <> " = " <> insertableValue v

requestToQuery schema (Node (Delete _ conditions, (mainTbl, _)) _) =
  query
  where
    qi = QualifiedIdentifier schema mainTbl
    query = T.unwords [
      "DELETE FROM ", fromQi qi,
      ("WHERE " <> T.intercalate " AND " ( map (pgFmtCondition qi ) conditions )) `emptyOnNull` conditions,
      "RETURNING " <> fromQi qi <> ".*"
      ]

selectStarF :: T.Text
selectStarF = "SELECT * FROM " <> sourceSubqueryName

sourceSubqueryName :: T.Text
sourceSubqueryName = "pg_source"

unquoted :: JSON.Value -> T.Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  cs $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = cs . show $ b
unquoted v = cs $ JSON.encode v

wrapQuery :: T.Text -> [T.Text] -> T.Text ->  Maybe NonnegRange -> T.Text
wrapQuery source selectColumns returnSelect range =
  withSourceF source <>
  " SELECT " <>
  T.intercalate ", " selectColumns <>
  " " <>
  fromF returnSelect ( limitF range )

-- private functions
fromQi :: QualifiedIdentifier -> T.Text
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
    s = tableSchema t
    tN = tableName t
    ftN = tableName ft
    ltN = fromMaybe "" (tableName <$> lt)
    toFilter :: Text -> Text -> Column -> Column -> Filter
    toFilter tb ftb c fc = Filter (colName c, Nothing) "=" (VForeignKey (QualifiedIdentifier s tb) (ForeignKey fc{colTable=(colTable fc){tableName=ftb}}))

emptyOnNull :: Text -> [a] -> Text
emptyOnNull val x = if null x then "" else val

orderF :: [OrderTerm] -> T.Text
orderF ts =
  if null ts
    then ""
    else "ORDER BY " <> clause
  where
    clause = T.intercalate "," (map queryTerm ts)
    queryTerm :: OrderTerm -> T.Text
    queryTerm t = " "
           <> cs (pgFmtIdent $ otTerm t) <> " "
           <> cs (otDirection t)         <> " "
           <> maybe "" cs (otNullOrder t) <> " "

insertableValue :: JSON.Value -> T.Text
insertableValue JSON.Null = "null"
insertableValue v = (<> "::unknown") . pgFmtLit $ unquoted v

whiteList :: T.Text -> T.Text
whiteList val = fromMaybe
  (cs (pgFmtLit val) <> "::unknown ")
  (find ((==) . T.toLower $ val) ["null","true","false"])

pgFmtColumn :: QualifiedIdentifier -> T.Text -> T.Text
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c = fromQi table <> "." <> pgFmtIdent c

pgFmtField :: QualifiedIdentifier -> Field -> T.Text
pgFmtField table (c, jp) = pgFmtColumn table c <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> T.Text
pgFmtSelectItem table (f@(_, jp), Nothing) = pgFmtField table f <> pgFmtAsJsonPath jp
pgFmtSelectItem table (f@(_, jp), Just cast ) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> pgFmtAsJsonPath jp

pgFmtCondition :: QualifiedIdentifier -> Filter -> T.Text
pgFmtCondition table (Filter (col,jp) ops val) =
  notOp <> " " <> sqlCol  <> " " <> pgFmtOperator opCode <> " " <>
    if opCode `elem` ["is","isnot"] then whiteList (getInner val) else sqlValue
  where
    headPredicate:rest = T.split (=='.') ops
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

pgFmtValue :: T.Text -> T.Text -> T.Text
pgFmtValue opCode val =
 case opCode of
   "like" -> unknownLiteral $ T.map star val
   "ilike" -> unknownLiteral $ T.map star val
   "in" -> "(" <> T.intercalate ", " (map unknownLiteral $ T.split (==',') val) <> ") "
   "notin" -> "(" <> T.intercalate ", " (map unknownLiteral $ T.split (==',') val) <> ") "
   "@@" -> "to_tsquery(" <> unknownLiteral val <> ") "
   _    -> unknownLiteral val
 where
   star c = if c == '*' then '%' else c
   unknownLiteral = (<> "::unknown ") . pgFmtLit

pgFmtOperator :: T.Text -> T.Text
pgFmtOperator opCode = fromMaybe "=" $ M.lookup opCode operatorsMap
  where
    operatorsMap = M.fromList operators

pgFmtJsonPath :: Maybe JsonPath -> T.Text
pgFmtJsonPath (Just [x]) = "->>" <> pgFmtLit x
pgFmtJsonPath (Just (x:xs)) = "->" <> pgFmtLit x <> pgFmtJsonPath ( Just xs )
pgFmtJsonPath _ = ""

pgFmtAsJsonPath :: Maybe JsonPath -> T.Text
pgFmtAsJsonPath Nothing = ""
pgFmtAsJsonPath (Just xx) = " AS " <> last xx

trimNullChars :: T.Text -> T.Text
trimNullChars = T.takeWhile (/= '\x0')

withSourceF :: T.Text -> T.Text
withSourceF s = "WITH " <> sourceSubqueryName <> " AS (" <> s <>")"

fromF :: T.Text -> T.Text -> T.Text
fromF sel limit = "FROM (" <> sel <> " " <> limit <> ") t"

limitF :: Maybe NonnegRange -> T.Text
limitF r  = "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit  = maybe "ALL" (cs . show) $ join $ rangeLimit <$> r
    offset = cs . show $ fromMaybe 0 $ rangeOffset <$> r
