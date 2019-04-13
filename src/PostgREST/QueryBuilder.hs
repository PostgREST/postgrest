{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-|
Module      : PostgREST.QueryBuilder
Description : PostgREST SQL generating functions.

This module provides functions to consume data types that
represent database objects (e.g. Relation, Schema, SqlQuery)
and produces SQL Statements.

Any function that outputs a SQL fragment should be in this module.
-}
module PostgREST.QueryBuilder (
    callProc
  , createReadStatement
  , createWriteStatement
  , pgFmtIdent
  , pgFmtLit
  , requestToQuery
  , requestToCountQuery
  , unquoted
  , ResultsWithCount
  , pgFmtSetLocal
  , pgFmtSetLocalSearchPath
  ) where

import qualified Hasql.Statement         as H
import qualified Hasql.Encoders          as HE
import qualified Hasql.Decoders          as HD

import qualified Data.Aeson              as JSON

import           PostgREST.RangeQuery    (rangeLimit, rangeOffset, allRange)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import qualified Data.Set                as S
import           Data.Text               (intercalate, unwords, replace, isInfixOf, toLower)
import qualified Data.Text as T          (map, takeWhile, null)
import qualified Data.Text.Encoding as T
import           Data.Tree               (Tree(..))
import           PostgREST.Types
import           Text.InterpolatedString.Perl6 (qc)
import qualified Data.ByteString.Char8   as BS
import           Data.Scientific         ( FPFormat (..)
                                         , formatScientific
                                         , isInteger
                                         )
import           Protolude hiding        ( intercalate, cast, replace)
import           PostgREST.ApiRequest    (PreferRepresentation (..))

{-| The generic query result format used by API responses. The location header
    is represented as a list of strings containing variable bindings like
    @"k1=eq.42"@, or the empty list if there is no location header.
-}
type ResultsWithCount = (Maybe Int64, Int64, [BS.ByteString], BS.ByteString)

standardRow :: HD.Row ResultsWithCount
standardRow = (,,,) <$> HD.nullableColumn HD.int8 <*> HD.column HD.int8
                    <*> HD.column header <*> HD.column HD.bytea
  where
    header = HD.array $ HD.dimension replicateM $ HD.element HD.bytea

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
  HD.rowMaybe standardRow

createReadStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool -> Maybe FieldName ->
                       H.Statement () ResultsWithCount
createReadStatement selectQuery countQuery isSingle countTotal asCsv binaryField =
  unicodeStatement sql HE.unit decodeStandard False
 where
  sql = [qc|
      WITH {sourceCTEName} AS ({selectQuery}) SELECT {cols}
      FROM ( SELECT * FROM {sourceCTEName}) _postgrest_t |]
  countResultF = if countTotal then "("<>countQuery<>")" else "null"
  cols = intercalate ", " [
      countResultF <> " AS total_result_set",
      "pg_catalog.count(_postgrest_t) AS page_total",
      noLocationF <> " AS header",
      bodyF <> " AS body"
    ]
  bodyF
    | asCsv = asCsvF
    | isSingle = asJsonSingleF
    | isJust binaryField = asBinaryF $ fromJust binaryField
    | otherwise = asJsonF


createWriteStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool ->
                        PreferRepresentation -> [Text] ->
                        H.Statement ByteString (Maybe ResultsWithCount)
createWriteStatement selectQuery mutateQuery wantSingle isInsert asCsv rep pKeys =
  unicodeStatement sql (HE.param HE.unknown) decodeStandardMay True

 where
  sql = case rep of
    None -> [qc|
      WITH {sourceCTEName} AS ({mutateQuery})
      SELECT '', 0, {noLocationF}, '' |]
    HeadersOnly -> [qc|
      WITH {sourceCTEName} AS ({mutateQuery})
      SELECT {cols}
      FROM (SELECT 1 FROM {sourceCTEName}) _postgrest_t |]
    Full -> [qc|
      WITH {sourceCTEName} AS ({mutateQuery})
      SELECT {cols}
      FROM ({selectQuery}) _postgrest_t |]

  cols = intercalate ", " [
      "'' AS total_result_set", -- when updateing it does not make sense
      "pg_catalog.count(_postgrest_t) AS page_total",
      if isInsert
        then unwords [
          "CASE",
            "WHEN pg_catalog.count(_postgrest_t) = 1 THEN",
              "coalesce(" <> locationF pKeys <> ", " <> noLocationF <> ")",
            "ELSE " <> noLocationF,
          "END AS header"]
        else noLocationF <> "AS header",
      if rep == Full
         then bodyF <> " AS body"
         else "''"
    ]

  bodyF
    | asCsv = asCsvF
    | wantSingle = asJsonSingleF
    | otherwise = asJsonF

type ProcResults = (Maybe Int64, Int64, ByteString, ByteString)
callProc :: QualifiedIdentifier -> [PgArg] -> Bool -> SqlQuery -> SqlQuery -> Bool ->
            Bool -> Bool -> Bool -> Bool -> Maybe FieldName -> PgVersion ->
            H.Statement ByteString (Maybe ProcResults)
callProc qi pgArgs returnsScalar selectQuery countQuery countTotal isSingle paramsAsSingleObject asCsv asBinary binaryField pgVer =
  unicodeStatement sql (HE.param HE.unknown) decodeProc True
  where
    sql =[qc|
      WITH
      {argsRecord},
      {sourceCTEName} AS (
        {sourceBody}
      )
      SELECT
        {countResultF} AS total_result_set,
        pg_catalog.count(_postgrest_t) AS page_total,
        {bodyF} AS body,
        {responseHeaders} AS response_headers
      FROM ({selectQuery}) _postgrest_t;|]

    (argsRecord, args)
      | paramsAsSingleObject = ("_args_record AS (SELECT NULL)", "$1::json")
      | null pgArgs = (ignoredBody, "")
      | otherwise = (
          unwords [
            normalizedBody <> ",",
            "_args_record AS (",
              "SELECT * FROM json_to_recordset(" <> selectBody <> ") AS _(" <>
                intercalate ", " ((\a -> pgFmtIdent (pgaName a) <> " " <> pgaType a) <$> pgArgs) <> ")",
            ")"]
         , intercalate ", " ((\a -> pgFmtIdent (pgaName a) <> " := _args_record." <> pgFmtIdent (pgaName a)) <$> pgArgs))

    sourceBody :: SqlFragment
    sourceBody
      | paramsAsSingleObject || null pgArgs =
          if returnsScalar
            then [qc| SELECT {fromQi qi}({args}) |]
            else [qc| SELECT * FROM {fromQi qi}({args}) |]
      | otherwise =
          if returnsScalar
            then [qc| SELECT {fromQi qi}({args}) FROM _args_record |]
            else [qc| SELECT _.*
                      FROM _args_record,
                      LATERAL ( SELECT * FROM {fromQi qi}({args}) ) _ |]

    bodyF
     | returnsScalar = scalarBodyF
     | isSingle = asJsonSingleF
     | asCsv = asCsvF
     | isJust binaryField = asBinaryF $ fromJust binaryField
     | otherwise = asJsonF

    scalarBodyF
     | asBinary = asBinaryF _procName
     | otherwise = unwords [
        "CASE",
          "WHEN pg_catalog.count(_postgrest_t) = 1",
            "THEN (json_agg(_postgrest_t." <> pgFmtIdent _procName <> ")->0)::character varying",
            "ELSE (json_agg(_postgrest_t." <> pgFmtIdent _procName <> "))::character varying",
        "END"]

    countResultF = if countTotal then "( "<> countQuery <> ")" else "null::bigint" :: Text
    _procName = qiName qi
    responseHeaders =
      if pgVer >= pgVersion96
        then "coalesce(nullif(current_setting('response.headers', true), ''), '[]')" :: Text -- nullif is used because of https://gist.github.com/steve-chavez/8d7033ea5655096903f3b52f8ed09a15
        else "'[]'" :: Text

    decodeProc = HD.rowMaybe procRow
    procRow = (,,,) <$> HD.nullableColumn HD.int8 <*> HD.column HD.int8
                    <*> HD.column HD.bytea <*> HD.column HD.bytea

pgFmtIdent :: SqlFragment -> SqlFragment
pgFmtIdent x = "\"" <> replace "\"" "\"\"" (trimNullChars $ toS x) <> "\""

pgFmtLit :: SqlFragment -> SqlFragment
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> replace "'" "''" trimmed <> "'"
     slashed = replace "\\" "\\\\" escaped in
 if "\\" `isInfixOf` escaped
   then "E" <> slashed
   else slashed

requestToCountQuery :: Schema -> DbRequest -> SqlQuery
requestToCountQuery _ (DbMutate _) = witness
requestToCountQuery schema (DbRead (Node (Select{where_=logicForest}, (mainTbl, _, _, _, _)) _)) =
 unwords [
   "SELECT pg_catalog.count(*)",
   "FROM ", fromQi qi,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest
   ]
 where
   qi = removeSourceCTESchema schema mainTbl

requestToQuery :: Schema -> Bool -> DbRequest -> SqlQuery
requestToQuery schema isParent (DbRead (Node (Select colSelects tbl tblAlias implJoins logicForest joinConditions_ ordts range, _) forest)) =
  unwords [
    "SELECT " <> intercalate ", " (map (pgFmtSelectItem qi) colSelects ++ selects),
    "FROM " <> intercalate ", " (tabl : implJs),
    unwords joins,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest ++ map pgFmtJoinCondition joinConditions_))
      `emptyOnFalse` (null logicForest && null joinConditions_),
    ("ORDER BY " <> intercalate ", " (map (pgFmtOrderTerm qi) ordts)) `emptyOnFalse` null ordts,
    ("LIMIT " <> maybe "ALL" show (rangeLimit range) <> " OFFSET " <> show (rangeOffset range)) `emptyOnFalse` (isParent || range == allRange) ]

  where
    implJs = fromQi . QualifiedIdentifier schema <$> implJoins
    mainQi = removeSourceCTESchema schema tbl
    tabl = fromQi mainQi <> maybe mempty (\a -> " AS " <> pgFmtIdent a) tblAlias
    qi = maybe mainQi (QualifiedIdentifier mempty) tblAlias

    (joins, selects) = foldr getQueryParts ([],[]) forest

    getQueryParts :: Tree ReadNode -> ([SqlFragment], [SqlFragment]) -> ([SqlFragment], [SqlFragment])
    getQueryParts (Node n@(_, (name, Just Relation{relType=Child,relTable=Table{tableName=table}}, alias, _, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE(("
           <> "SELECT json_agg(" <> pgFmtIdent table <> ".*) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = requestToQuery schema False (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just Relation{relType=Parent,relTable=Table{tableName=table}}, alias, _, _)) forst) (j,s) = (joi:j,sel:s)
      where
        aliasOrName = fromMaybe name alias
        localTableName = pgFmtIdent $ table <> "_" <> aliasOrName
        sel = "row_to_json(" <> localTableName <> ".*) AS " <> pgFmtIdent aliasOrName
        joi = " LEFT JOIN LATERAL( " <> subquery <> " ) AS " <> localTableName <> " ON TRUE "
          where subquery = requestToQuery schema True (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just Relation{relType=Many,relTable=Table{tableName=table}}, alias, _, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE (("
           <> "SELECT json_agg(" <> pgFmtIdent table <> ".*) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = requestToQuery schema False (DbRead (Node n forst))
    --the following is just to remove the warning
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts _ _ = witness
requestToQuery schema _ (DbMutate (Insert mainTbl iCols onConflct putConditions returnings)) =
  unwords [
    "WITH " <> normalizedBody,
    "INSERT INTO ", fromQi qi, if S.null iCols then " " else "(" <> cols <> ")",
    unwords [
      "SELECT " <> cols <> " FROM",
      "json_populate_recordset", "(null::", fromQi qi, ", " <> selectBody <> ") _",
      -- Only used for PUT
      ("WHERE " <> intercalate " AND " (pgFmtLogicTree (QualifiedIdentifier "" "_") <$> putConditions)) `emptyOnFalse` null putConditions],
    maybe "" (\(oncDo, oncCols) -> (
      "ON CONFLICT(" <> intercalate ", " (pgFmtIdent <$> oncCols) <> ") " <> case oncDo of
      IgnoreDuplicates ->
        "DO NOTHING"
      MergeDuplicates  ->
        if S.null iCols
           then "DO NOTHING"
           else "DO UPDATE SET " <> intercalate ", " (pgFmtIdent <> const " = EXCLUDED." <> pgFmtIdent <$> S.toList iCols)
                                   ) `emptyOnFalse` null oncCols) onConflct,
    ("RETURNING " <> intercalate ", " (map (pgFmtColumn qi) returnings)) `emptyOnFalse` null returnings]
  where
    qi = QualifiedIdentifier schema mainTbl
    cols = intercalate ", " $ pgFmtIdent <$> S.toList iCols
requestToQuery schema _ (DbMutate (Update mainTbl uCols logicForest returnings)) =
  if S.null uCols
    then "WITH " <> ignoredBody <> "SELECT null WHERE false" -- if there are no columns we cannot do UPDATE table SET {empty}, it'd be invalid syntax
    else
      unwords [
        "WITH " <> normalizedBody,
        "UPDATE " <> fromQi qi <> " SET " <> cols,
        "FROM (SELECT * FROM json_populate_recordset", "(null::", fromQi qi, ", " <> selectBody <> ")) _ ",
        ("WHERE " <> intercalate " AND " (pgFmtLogicTree qi <$> logicForest)) `emptyOnFalse` null logicForest,
        ("RETURNING " <> intercalate ", " (pgFmtColumn qi <$> returnings)) `emptyOnFalse` null returnings
        ]
  where
    qi = QualifiedIdentifier schema mainTbl
    cols = intercalate ", " (pgFmtIdent <> const " = _." <> pgFmtIdent <$> S.toList uCols)
requestToQuery schema _ (DbMutate (Delete mainTbl logicForest returnings)) =
  unwords [
    "WITH " <> ignoredBody,
    "DELETE FROM ", fromQi qi,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest,
    ("RETURNING " <> intercalate ", " (map (pgFmtColumn qi) returnings)) `emptyOnFalse` null returnings
    ]
  where
    qi = QualifiedIdentifier schema mainTbl

-- Due to the use of the `unknown` encoder we need to cast '$1' when the value is not used in the main query
-- otherwise the query will err with a `could not determine data type of parameter $1`.
-- This happens because `unknown` relies on the context to determine the value type.
-- The error also happens on raw libpq used with C.
ignoredBody :: SqlFragment
ignoredBody = "ignored_body AS (SELECT $1::text) "

-- |
-- These CTEs convert a json object into a json array, this way we can use json_populate_recordset for all json payloads
-- Otherwise we'd have to use json_populate_record for json objects and json_populate_recordset for json arrays
-- We do this in SQL to avoid processing the JSON in application code
normalizedBody :: SqlFragment
normalizedBody =
  unwords [
    "pgrst_payload AS (SELECT $1::json AS json_data),",
    "pgrst_body AS (",
      "SELECT",
        "CASE WHEN json_typeof(json_data) = 'array'",
          "THEN json_data",
          "ELSE json_build_array(json_data)",
        "END AS val",
      "FROM pgrst_payload)"]

selectBody :: SqlFragment
selectBody = "(SELECT val FROM pgrst_body)"

removeSourceCTESchema :: Schema -> TableName -> QualifiedIdentifier
removeSourceCTESchema schema tbl = QualifiedIdentifier (if tbl == sourceCTEName then "" else schema) tbl

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = toS $ JSON.encode v

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
    asCsvBodyF = "coalesce(string_agg(substring(_postgrest_t::text, 2, length(_postgrest_t::text) - 2), '\n'), '')"

asJsonF :: SqlFragment
asJsonF = "coalesce(json_agg(_postgrest_t), '[]')::character varying"

asJsonSingleF :: SqlFragment --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF = "coalesce(string_agg(row_to_json(_postgrest_t)::text, ','), '')::character varying "

asBinaryF :: FieldName -> SqlFragment
asBinaryF fieldName = "coalesce(string_agg(_postgrest_t." <> pgFmtIdent fieldName <> ", ''), '')"

locationF :: [Text] -> SqlFragment
locationF pKeys = [qc|(
  WITH data AS (SELECT row_to_json(_) AS row FROM {sourceCTEName} AS _ LIMIT 1)
  SELECT array_agg(json_data.key || '=' || coalesce('eq.' || json_data.value, 'is.null'))
  FROM data CROSS JOIN json_each_text(data.row) AS json_data
  {("WHERE json_data.key IN ('" <> intercalate "','" pKeys <> "')") `emptyOnFalse` null pKeys}
)|]

fromQi :: QualifiedIdentifier -> SqlFragment
fromQi t = (if s == "" then "" else pgFmtIdent s <> ".") <> pgFmtIdent n
  where
    n = qiName t
    s = qiSchema t

unicodeStatement :: Text -> HE.Params a -> HD.Result b -> Bool -> H.Statement a b
unicodeStatement = H.Statement . T.encodeUtf8

emptyOnFalse :: Text -> Bool -> Text
emptyOnFalse val cond = if cond then "" else val

pgFmtColumn :: QualifiedIdentifier -> Text -> SqlFragment
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c = fromQi table <> "." <> pgFmtIdent c

pgFmtField :: QualifiedIdentifier -> Field -> SqlFragment
pgFmtField table (c, jp) = pgFmtColumn table c <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> SqlFragment
pgFmtSelectItem table (f@(fName, jp), Nothing, alias, _) = pgFmtField table f <> pgFmtAs fName jp alias
pgFmtSelectItem table (f@(fName, jp), Just cast, alias, _) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> pgFmtAs fName jp alias

pgFmtOrderTerm :: QualifiedIdentifier -> OrderTerm -> SqlFragment
pgFmtOrderTerm qi ot = unwords [
  toS . pgFmtField qi $ otTerm ot,
  maybe "" show $ otDirection ot,
  maybe "" show $ otNullOrder ot]

pgFmtFilter :: QualifiedIdentifier -> Filter -> SqlFragment
pgFmtFilter table (Filter fld (OpExpr hasNot oper)) = notOp <> " " <> case oper of
   Op op val  -> pgFmtFieldOp op <> " " <> case op of
     "like"   -> unknownLiteral (T.map star val)
     "ilike"  -> unknownLiteral (T.map star val)
     "is"     -> whiteList val
     _        -> unknownLiteral val

   In vals -> pgFmtField table fld <> " " <>
    let emptyValForIn = "= any('{}') " in -- Workaround because for postgresql "col IN ()" is invalid syntax, we instead do "col = any('{}')"
    case (&&) (length vals == 1) . T.null <$> headMay vals of
      Just False -> sqlOperator "in" <> "(" <> intercalate ", " (map unknownLiteral vals) <> ") "
      Just True  -> emptyValForIn
      Nothing    -> emptyValForIn

   Fts op lang val ->
     pgFmtFieldOp op
       <> "("
       <> maybe "" ((<> ", ") . pgFmtLit) lang
       <> unknownLiteral val
       <> ") "
 where
   pgFmtFieldOp op = pgFmtField table fld <> " " <> sqlOperator op
   sqlOperator o = HM.lookupDefault "=" o operators
   notOp = if hasNot then "NOT" else ""
   star c = if c == '*' then '%' else c
   unknownLiteral = (<> "::unknown ") . pgFmtLit
   whiteList :: Text -> SqlFragment
   whiteList v = fromMaybe
     (toS (pgFmtLit v) <> "::unknown ")
     (find ((==) . toLower $ v) ["null","true","false"])

pgFmtJoinCondition :: JoinCondition -> SqlFragment
pgFmtJoinCondition (JoinCondition (qi, col1) (QualifiedIdentifier schema fTable, col2)) =
  pgFmtColumn qi col1 <> " = " <>
  pgFmtColumn (removeSourceCTESchema schema fTable) col2

pgFmtLogicTree :: QualifiedIdentifier -> LogicTree -> SqlFragment
pgFmtLogicTree qi (Expr hasNot op forest) = notOp <> " (" <> intercalate (" " <> show op <> " ") (pgFmtLogicTree qi <$> forest) <> ")"
  where notOp =  if hasNot then "NOT" else ""
pgFmtLogicTree qi (Stmnt flt) = pgFmtFilter qi flt

pgFmtJsonPath :: JsonPath -> SqlFragment
pgFmtJsonPath = \case
  []             -> ""
  (JArrow x:xs)  -> "->" <> pgFmtJsonOperand x <> pgFmtJsonPath xs
  (J2Arrow x:xs) -> "->>" <> pgFmtJsonOperand x <> pgFmtJsonPath xs
  where
    pgFmtJsonOperand (JKey k) = pgFmtLit k
    pgFmtJsonOperand (JIdx i) = pgFmtLit i <> "::int"

pgFmtAs :: FieldName -> JsonPath -> Maybe Alias -> SqlFragment
pgFmtAs _ [] Nothing = ""
pgFmtAs fName jp Nothing = case jOp <$> lastMay jp of
  Just (JKey key) -> " AS " <> pgFmtIdent key
  Just (JIdx _)   -> " AS " <> pgFmtIdent (fromMaybe fName lastKey)
    -- We get the lastKey because on:
    -- `select=data->1->mycol->>2`, we need to show the result as [ {"mycol": ..}, {"mycol": ..} ]
    -- `select=data->3`, we need to show the result as [ {"data": ..}, {"data": ..} ]
    where lastKey = jVal <$> find (\case JKey{} -> True; _ -> False) (jOp <$> reverse jp)
  Nothing -> ""
pgFmtAs _ _ (Just alias) = " AS " <> pgFmtIdent alias

pgFmtSetLocal :: Text -> (Text, Text) -> SqlFragment
pgFmtSetLocal prefix (k, v) =
  "SET LOCAL " <> pgFmtIdent (prefix <> k) <> " = " <> pgFmtLit v <> ";"

pgFmtSetLocalSearchPath :: [Text] -> SqlFragment
pgFmtSetLocalSearchPath vals =
  "SET LOCAL search_path = " <> intercalate ", " (pgFmtLit <$> vals) <> ";"

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')
