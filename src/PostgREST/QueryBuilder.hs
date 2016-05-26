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
  , ResultsWithCount
  ) where

import qualified Hasql.Query             as H
import qualified Hasql.Encoders          as HE
import qualified Hasql.Decoders          as HD

import qualified Data.Aeson              as JSON
import           Data.Int                (Int64)

import           PostgREST.RangeQuery    (NonnegRange, rangeLimit, rangeOffset)
import           Control.Error           (note, fromMaybe)
import           Data.Functor.Contravariant (contramap)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (find)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, intercalate, unwords, replace, isInfixOf, toLower, split)
import qualified Data.Text as T          (map, takeWhile)
import qualified Data.Text.Encoding as T
import           Data.String.Conversions (cs)
import           Control.Applicative     ((<|>))
import           Control.Monad           (replicateM)
import           Data.Tree               (Tree(..))
import qualified Data.Vector as V
import           PostgREST.Types
import qualified Data.Map as M
import           Text.InterpolatedString.Perl6 (qc)
import           Text.Regex.TDFA         ((=~))
import qualified Data.ByteString.Char8   as BS
import           Data.Scientific         ( FPFormat (..)
                                         , formatScientific
                                         , isInteger
                                         )
import           Prelude hiding          (unwords)
import           PostgREST.ApiRequest    (PreferRepresentation (..))

{-| The generic query result format used by API responses. The location header
    is represented as a list of strings containing variable bindings like
    @"k1=eq.42"@, or the empty list if there is no location header.
-}
type ResultsWithCount = (Maybe Int64, Int64, [BS.ByteString], BS.ByteString)

standardRow :: HD.Row ResultsWithCount
standardRow = (,,,) <$> HD.nullableValue HD.int8 <*> HD.value HD.int8
                    <*> HD.value header <*> HD.value HD.bytea
  where
    header = HD.array $ HD.arrayDimension replicateM $ HD.arrayValue HD.bytea

noLocationF :: Text
noLocationF = "array[]::text[]"

{-| Read and Write api requests use a similar response format which includes
    various record counts and possible location header. This is the decoder
    for that common type of query.
-}
decodeStandard :: HD.Result ResultsWithCount
decodeStandard =
  HD.singleRow standardRow

decodeStandardMay :: HD.Result (Maybe ResultsWithCount)
decodeStandardMay =
  HD.maybeRow standardRow

{-| JSON and CSV payloads from the client are given to us as
    UniformObjects (objects who all have the same keys),
    and we turn this into an old fasioned JSON array
-}
encodeUniformObjs :: HE.Params UniformObjects
encodeUniformObjs =
  contramap (JSON.Array . V.map JSON.Object . unUniformObjects) (HE.value HE.json)

createReadStatement :: SqlQuery -> SqlQuery -> NonnegRange -> Bool -> Bool -> Bool ->
                       H.Query () ResultsWithCount
createReadStatement selectQuery countQuery range isSingle countTotal asCsv =
  unicodeStatement sql HE.unit decodeStandard True
 where
  sql = [qc|
      WITH {sourceCTEName} AS ({selectQuery}) SELECT {cols}
      FROM ( SELECT * FROM {sourceCTEName} {limitF range}) t |]
  countResultF = if countTotal then "("<>countQuery<>")" else "null"
  cols = intercalate ", " [
      countResultF <> " AS total_result_set",
      "pg_catalog.count(t) AS page_total",
      noLocationF <> " AS header",
      bodyF <> " AS body"
    ]
  bodyF
    | asCsv = asCsvF
    | isSingle = asJsonSingleF
    | otherwise = asJsonF

createWriteStatement :: QualifiedIdentifier -> SqlQuery -> SqlQuery -> Bool ->
                        PreferRepresentation -> [Text] -> Bool -> Payload ->
                        H.Query UniformObjects (Maybe ResultsWithCount)
createWriteStatement _ _ _ _ _ _ _ (PayloadParseError _) = undefined
createWriteStatement _ _ mutateQuery _ None
                     _ _ (PayloadJSON (UniformObjects _)) =
  unicodeStatement sql encodeUniformObjs decodeStandardMay True
 where
  sql = [qc|
      WITH {sourceCTEName} AS ({mutateQuery})
      SELECT '', 0, {noLocationF}, '' |]

createWriteStatement qi _ mutateQuery isSingle HeadersOnly
                     pKeys _ (PayloadJSON (UniformObjects _)) =
  unicodeStatement sql encodeUniformObjs decodeStandardMay True
 where
  sql = [qc|
      WITH {sourceCTEName} AS ({mutateQuery} RETURNING {fromQi qi}.*)
      SELECT {cols}
      FROM (SELECT 1 FROM {sourceCTEName}) t |]
  cols = intercalate ", " [
      "'' AS total_result_set",
      "pg_catalog.count(t) AS page_total",
      if isSingle then locationF pKeys else noLocationF,
      "''"
    ]

createWriteStatement qi selectQuery mutateQuery isSingle Full
                     pKeys asCsv (PayloadJSON (UniformObjects _)) =
  unicodeStatement sql encodeUniformObjs decodeStandardMay True
 where
  sql = [qc|
      WITH {sourceCTEName} AS ({mutateQuery} RETURNING {fromQi qi}.*)
      SELECT {cols}
      FROM ({selectQuery}) t |]
  cols = intercalate ", " [
      "'' AS total_result_set", -- when updateing it does not make sense
      "pg_catalog.count(t) AS page_total",
      if isSingle then locationF pKeys else noLocationF <> " AS header",
      bodyF <> " AS body"
    ]
  bodyF
    | asCsv = asCsvF
    | isSingle = asJsonSingleF
    | otherwise = asJsonF

addRelations :: Schema -> [Relation] -> Maybe ReadRequest -> ReadRequest -> Either Text ReadRequest
addRelations schema allRelations parentNode node@(Node readNode@(query, (name, _, alias)) forest) =
  case parentNode of
    (Just (Node (Select{from=[parentTable]}, (_, _, _)) _)) -> Node <$> (addRel readNode <$> rel) <*> updatedForest
      where
        rel = note ("no relation between " <> parentTable <> " and " <> name)
            $  findRelationByTable schema name parentTable
           <|> findRelationByColumn schema parentTable name
        addRel :: (ReadQuery, (NodeName, Maybe Relation, Maybe Alias)) -> Relation -> (ReadQuery, (NodeName, Maybe Relation, Maybe Alias))
        addRel (query', (n, _, a)) r = (query' {from=fromRelation}, (n, Just r, a))
          where fromRelation = map (\t -> if t == n then tableName (relTable r) else t) (from query')

    _ -> Node (query, (name, Nothing, alias)) <$> updatedForest
  where
    updatedForest = mapM (addRelations schema allRelations (Just node)) forest
    -- Searches through all the relations and returns a match given the parameter conditions.
    -- Will only find a relation where both schemas are in the PostgREST schema.
    -- `findRelationByColumn` also does a ducktype check to see if the column name has any variation of `id` or `fk`. If so then the relation is returned as a match.
    findRelationByTable s t1 t2 =
      find (\r -> s == tableSchema (relTable r) && s == tableSchema (relFTable r) && t1 == tableName (relTable r) && t2 == tableName (relFTable r)) allRelations
    findRelationByColumn s t c =
      find (\r -> s == tableSchema (relTable r) && s == tableSchema (relFTable r) && t == tableName (relFTable r) && length (relFColumns r) == 1 && c `colMatches` (colName . head . relFColumns) r) allRelations
      where n `colMatches` rc = (cs ("^" <> rc <> "_?(?:|[iI][dD]|[fF][kK])$") :: BS.ByteString) =~ (cs n :: BS.ByteString)

addJoinConditions :: Schema -> ReadRequest -> Either Text ReadRequest
addJoinConditions schema (Node nn@(query, (n, r, a)) forest) =
  case r of
    Nothing -> Node nn  <$> updatedForest -- this is the root node
    Just rel@Relation{relType=Child} -> Node (addCond query (getJoinConditions rel),(n,r,a)) <$> updatedForest
    Just Relation{relType=Parent} -> Node nn <$> updatedForest
    Just rel@Relation{relType=Many, relLTable=(Just linkTable)} ->
      Node (qq, (n, r, a)) <$> updatedForest
      where
         query' = addCond query (getJoinConditions rel)
         qq = query'{from=tableName linkTable : from query'}
    _ -> Left "unknown relation"
  where
    updatedForest = mapM (addJoinConditions schema) forest
    addCond query' con = query'{flt_=con ++ flt_ query'}

type ProcResults = (Maybe Int64, Int64, JSON.Value)
callProc :: QualifiedIdentifier -> JSON.Object -> NonnegRange -> Bool -> H.Query () (Maybe ProcResults)
callProc qi params range countTotal =
  unicodeStatement sql HE.unit decodeProc True
  where
    sql = [qc|
            WITH t AS (select * {_callSql})
            SELECT
              {_countExpr} as countTotal,
              pg_catalog.count(1) as countResult,
              array_to_json(
                coalesce(array_agg(row_to_json(r)), '\{}')
              )::character varying
            FROM (select * from t {limitF range}) r;
          |]
    _args = intercalate "," $ map _assignment (HM.toList params)
    _assignment (n,v) = pgFmtIdent n <> ":=" <> insertableValue v
    _callSql = [qc| from {fromQi qi}({_args}) |] :: Text
    _countExpr = if countTotal
                   then "(select pg_catalog.count(1) from t)"
                   else "null::bigint" :: Text
    decodeProc = HD.maybeRow procRow
    procRow = (,,) <$> HD.nullableValue HD.int8 <*> HD.value HD.int8
                   <*> HD.value HD.json

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
 if "\\" `isInfixOf` escaped
   then "E" <> slashed
   else slashed

requestToCountQuery :: Schema -> DbRequest -> SqlQuery
requestToCountQuery _ (DbMutate _) = undefined
requestToCountQuery schema (DbRead (Node (Select _ _ conditions _, (mainTbl, _, _)) _)) =
 unwords [
   "SELECT pg_catalog.count(1)",
   "FROM ", fromQi $ QualifiedIdentifier schema mainTbl,
   ("WHERE " <> intercalate " AND " ( map (pgFmtCondition (QualifiedIdentifier schema mainTbl)) localConditions )) `emptyOnNull` localConditions
   ]
 where
   fn Filter{value=VText _} = True
   fn Filter{value=VForeignKey _ _} = False
   localConditions = filter fn conditions

requestToQuery :: Schema -> DbRequest -> SqlQuery
requestToQuery _ (DbMutate (Insert _ (PayloadParseError _))) = undefined
requestToQuery _ (DbMutate (Update _ (PayloadParseError _) _)) = undefined
requestToQuery schema (DbRead (Node (Select colSelects tbls conditions ord, (nodeName, maybeRelation, _)) forest)) =
  query
  where
    -- TODO! the folloing helper functions are just to remove the "schema" part when the table is "source" which is the name
    -- of our WITH query part
    mainTbl = fromMaybe nodeName (tableName . relTable <$> maybeRelation)
    tblSchema tbl = if tbl == sourceCTEName then "" else schema
    qi = QualifiedIdentifier (tblSchema mainTbl) mainTbl
    toQi t = QualifiedIdentifier (tblSchema t) t
    query = unwords [
      "SELECT ", intercalate ", " (map (pgFmtSelectItem qi) colSelects ++ selects),
      "FROM ", intercalate ", " (map (fromQi . toQi) tbls),
      unwords joins,
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
    (joins, selects) = foldr getQueryParts ([],[]) forest

    getQueryParts :: Tree ReadNode -> ([SqlFragment], [SqlFragment]) -> ([SqlFragment], [SqlFragment])
    getQueryParts (Node n@(_, (name, Just Relation{relType=Child,relTable=Table{tableName=table}}, alias)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE(("
           <> "SELECT array_to_json(array_agg(row_to_json("<>pgFmtIdent table<>"))) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = requestToQuery schema (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just r@Relation{relType=Parent,relTable=Table{tableName=table}}, alias)) forst) (j,s) = (joi:j,sel:s)
      where
        node_name = fromMaybe name alias
        local_table_name = table <> "_" <> node_name
        replaceTableName localTableName (Filter a b (VForeignKey (QualifiedIdentifier "" _) c)) = Filter a b (VForeignKey (QualifiedIdentifier "" localTableName) c)
        replaceTableName _ x = x
        sel = "row_to_json(" <> pgFmtIdent local_table_name <> ".*) AS " <> pgFmtIdent node_name
        joi = " LEFT OUTER JOIN ( " <> subquery <> " ) AS " <> pgFmtIdent local_table_name  <>
              " ON " <> intercalate " AND " ( map (pgFmtCondition qi . replaceTableName local_table_name) (getJoinConditions r) )
          where subquery = requestToQuery schema (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just Relation{relType=Many,relTable=Table{tableName=table}}, alias)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE (("
           <> "SELECT array_to_json(array_agg(row_to_json("<>pgFmtIdent table<>"))) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = requestToQuery schema (DbRead (Node n forst))
    --the following is just to remove the warning
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts (Node (_,(_,Nothing,_)) _) _ = undefined
requestToQuery schema (DbMutate (Insert mainTbl (PayloadJSON (UniformObjects rows)))) =
  let qi = QualifiedIdentifier schema mainTbl
      cols = map pgFmtIdent $ fromMaybe [] (HM.keys <$> (rows V.!? 0))
      colsString = intercalate ", " cols in
  unwords [
    "INSERT INTO ", fromQi qi,
    " (" <> colsString <> ")" <>
    " SELECT " <> colsString <>
    " FROM json_populate_recordset(null::" , fromQi qi, ", $1)"
    ]
requestToQuery schema (DbMutate (Update mainTbl (PayloadJSON (UniformObjects rows)) conditions)) =
  case rows V.!? 0 of
    Just obj ->
      let assignments = map
            (\(k,v) -> pgFmtIdent k <> "=" <> insertableValue v) $ HM.toList obj in
      unwords [
        "UPDATE ", fromQi qi,
        " SET " <> intercalate "," assignments <> " ",
        ("WHERE " <> intercalate " AND " ( map (pgFmtCondition qi ) conditions )) `emptyOnNull` conditions
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
      ("WHERE " <> intercalate " AND " ( map (pgFmtCondition qi ) conditions )) `emptyOnNull` conditions
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
      "(SELECT coalesce(string_agg(a.k, ','), '')" <>
      "  FROM (" <>
      "    SELECT json_object_keys(r)::TEXT as k" <>
      "    FROM ( " <>
      "      SELECT row_to_json(hh) as r from " <> sourceCTEName <> " as hh limit 1" <>
      "    ) s" <>
      "  ) a" <>
      ")"
    asCsvBodyF = "coalesce(string_agg(substring(t::text, 2, length(t::text) - 2), '\n'), '')"

asJsonF :: SqlFragment
asJsonF = "coalesce(array_to_json(array_agg(row_to_json(t))), '[]')::character varying"

asJsonSingleF :: SqlFragment --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF = "coalesce(string_agg(row_to_json(t)::text, ','), '')::character varying "

locationF :: [Text] -> SqlFragment
locationF pKeys =
    "(" <>
    " WITH s AS (SELECT row_to_json(ss) as r from " <> sourceCTEName <> " as ss  limit 1)" <>
    " SELECT array_agg(json_data.key || '=' || coalesce('eq.' || json_data.value, 'is.null'))" <>
    " FROM s, json_each_text(s.r) AS json_data" <>
    (
      if null pKeys
      then ""
      else " WHERE json_data.key IN ('" <> intercalate "','" pKeys <> "')"
    ) <> ")"

limitF :: NonnegRange -> SqlFragment
limitF r  = "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit  = maybe "ALL" (cs . show) $ rangeLimit r
    offset = cs . show $ rangeOffset r

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

unicodeStatement :: Text -> HE.Params a -> HD.Result b -> Bool -> H.Query a b
unicodeStatement = H.statement . T.encodeUtf8

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
pgFmtSelectItem table (f@(_, jp), Nothing, alias) = pgFmtField table f <> pgFmtAs jp alias
pgFmtSelectItem table (f@(_, jp), Just cast, alias) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> pgFmtAs jp alias

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
        where qi = QualifiedIdentifier (if ft == sourceCTEName then "" else s) ft
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

pgFmtAs :: Maybe JsonPath -> Maybe Alias -> SqlFragment
pgFmtAs Nothing Nothing = ""
pgFmtAs (Just xx) Nothing = " AS " <> pgFmtIdent (last xx)
pgFmtAs _ (Just alias) = " AS " <> pgFmtIdent alias

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')
