{-# LANGUAGE FlexibleInstances    #-}
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
    callProc
  , createReadStatement
  , createWriteStatement
  , pgFmtIdent
  , pgFmtLit
  , requestToQuery
  , requestToCountQuery
  , unquoted
  , ResultsWithCount
  , pgFmtEnvVar
  ) where

import qualified Hasql.Query             as H
import qualified Hasql.Encoders          as HE
import qualified Hasql.Decoders          as HD

import qualified Data.Aeson              as JSON

import           PostgREST.Config        (pgVersion96)
import           PostgREST.RangeQuery    (NonnegRange, rangeLimit, rangeOffset, allRange)
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
import           Protolude hiding        (from, intercalate, ord, cast, replace)
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

createReadStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool -> Maybe FieldName ->
                       H.Query () ResultsWithCount
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
                        H.Query ByteString (Maybe ResultsWithCount)
createWriteStatement selectQuery mutateQuery wantSingle wantHdrs asCsv rep pKeys =
  unicodeStatement sql (HE.value HE.unknown) decodeStandardMay True

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
      if wantHdrs
         then locationF pKeys
         else noLocationF <> " AS header",
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
            Bool -> Bool -> Bool -> Bool -> Maybe FieldName -> Bool -> PgVersion ->
            H.Query ByteString (Maybe ProcResults)
callProc qi pgArgs returnsScalar selectQuery countQuery countTotal isSingle paramsAsSingleObject asCsv asBinary binaryField isObject pgVer =
  unicodeStatement sql (HE.value HE.unknown) decodeProc True
  where
    sql =
     if returnsScalar then [qc|
       WITH {argsRecord},
       {sourceCTEName} AS (
         SELECT {fromQi qi}({args})
       )
       SELECT
         {countResultF} AS total_result_set,
         1 AS page_total,
         {scalarBodyF} AS body,
         {responseHeaders} AS response_headers
       FROM ({selectQuery}) _postgrest_t;|]
     else [qc|
       WITH {argsRecord},
       {sourceCTEName} AS (
         SELECT * FROM {fromQi qi}({args})
       )
       SELECT
         {countResultF} AS total_result_set,
         pg_catalog.count(_postgrest_t) AS page_total,
         {bodyF} AS body,
         {responseHeaders} AS response_headers
       FROM ({selectQuery}) _postgrest_t;|]

    (argsRecord, args) | paramsAsSingleObject = ("_args_record AS (SELECT NULL)", "$1::json")
                       | null pgArgs = (ignoredBody, "")
                       | otherwise = (
                           unwords [
                           "_args_record AS (",
                             "SELECT * FROM " <> (if isObject then "json_to_record" else "json_to_recordset") <> "($1)",
                             "AS _(" <> intercalate ", " ((\a -> pgaName a <> " " <> pgaType a) <$> pgArgs) <> ")",
                           ")"]
                         , intercalate ", " ((\a -> pgaName a <> " := (SELECT " <> pgaName a <> " FROM _args_record)") <$> pgArgs))
    countResultF = if countTotal then "( "<> countQuery <> ")" else "null::bigint" :: Text
    _procName = qiName qi
    responseHeaders =
      if pgVer >= pgVersion96
        then "coalesce(nullif(current_setting('response.headers', true), ''), '[]')" :: Text -- nullif is used because of https://gist.github.com/steve-chavez/8d7033ea5655096903f3b52f8ed09a15
        else "'[]'" :: Text
    decodeProc = HD.maybeRow procRow
    procRow = (,,,) <$> HD.nullableValue HD.int8 <*> HD.value HD.int8
                    <*> HD.value HD.bytea <*> HD.value HD.bytea
    scalarBodyF
     | asBinary = asBinaryF _procName
     | otherwise = "(row_to_json(_postgrest_t)->" <> pgFmtLit _procName <> ")::character varying"

    bodyF
     | isSingle = asJsonSingleF
     | asCsv = asCsvF
     | isJust binaryField = asBinaryF $ fromJust binaryField
     | otherwise = asJsonF

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
requestToCountQuery _ (DbMutate _) = undefined
requestToCountQuery schema (DbRead (Node (Select _ _ logicForest _ _, (mainTbl, _, _, _)) _)) =
 unwords [
   "SELECT pg_catalog.count(*)",
   "FROM ", fromQi qi,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) filteredLogic)) `emptyOnFalse` null filteredLogic
   ]
 where
   qi = removeSourceCTESchema schema mainTbl
   -- all foreing key filters are root nodes(see addFilterToLogicForest), only those are filtered
   nonFKRoot :: LogicTree -> Bool
   nonFKRoot (Stmnt (Filter _ (OpExpr _ (Join _ _)))) = False
   nonFKRoot (Stmnt _) = True
   nonFKRoot Expr{} = True
   filteredLogic = filter nonFKRoot logicForest

requestToQuery :: Schema -> Bool -> DbRequest -> SqlQuery
requestToQuery schema isParent (DbRead (Node (Select colSelects tbls logicForest ord range, (nodeName, maybeRelation, _, _)) forest)) =
  query
  where
    mainTbl = fromMaybe nodeName (tableName . relTable <$> maybeRelation)
    qi = removeSourceCTESchema schema mainTbl
    toQi = removeSourceCTESchema schema
    query = unwords [
      "SELECT ", intercalate ", " (map (pgFmtSelectItem qi) colSelects ++ selects),
      "FROM ", intercalate ", " (map (fromQi . toQi) tbls),
      unwords joins,
      ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest,
      orderF (fromMaybe [] ord),
      if isParent then "" else limitF range
      ]
    orderF ts =
        if null ts
            then ""
            else "ORDER BY " <> clause
        where
            clause = intercalate "," (map queryTerm ts)
            queryTerm :: OrderTerm -> Text
            queryTerm t = " "
                <> toS (pgFmtField qi $ otTerm t) <> " "
                <> maybe "" show (otDirection t) <> " "
                <> maybe "" show (otNullOrder t) <> " "
    (joins, selects) = foldr getQueryParts ([],[]) forest

    getQueryParts :: Tree ReadNode -> ([SqlFragment], [SqlFragment]) -> ([SqlFragment], [SqlFragment])
    getQueryParts (Node n@(_, (name, Just Relation{relType=Child,relTable=Table{tableName=table}}, alias, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE(("
           <> "SELECT json_agg(" <> pgFmtIdent table <> ".*) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = requestToQuery schema False (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just Relation{relType=Parent,relTable=Table{tableName=table}}, alias, _)) forst) (j,s) = (joi:j,sel:s)
      where
        aliasOrName = fromMaybe name alias
        localTableName = pgFmtIdent $ table <> "_" <> aliasOrName
        sel = "row_to_json(" <> localTableName <> ".*) AS " <> pgFmtIdent aliasOrName
        joi = " LEFT JOIN LATERAL( " <> subquery <> " ) AS " <> localTableName <> " ON TRUE "
          where subquery = requestToQuery schema True (DbRead (Node n forst))
    getQueryParts (Node n@(_, (name, Just Relation{relType=Many,relTable=Table{tableName=table}}, alias, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE (("
           <> "SELECT json_agg(" <> pgFmtIdent table <> ".*) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = requestToQuery schema False (DbRead (Node n forst))
    --the following is just to remove the warning
    --getQueryParts is not total but requestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts _ _ = undefined
requestToQuery schema _ (DbMutate (Insert mainTbl p@(PayloadJSON _ pType keys) returnings)) =
  unwords [
    ("WITH " <> ignoredBody) `emptyOnFalse` not payloadIsEmpty,
    "INSERT INTO ", fromQi qi, if payloadIsEmpty then " " else "(" <> cols <> ") ",
    case (pType, payloadIsEmpty) of
      (PJArray _, True) -> "SELECT null WHERE false"
      (PJObject, True)  -> "DEFAULT VALUES"
      _ -> unwords [
        "SELECT " <> cols <> " FROM ",
        case pType of
          PJObject  -> "json_populate_record"
          PJArray _ -> "json_populate_recordset", "(null::", fromQi qi, ", $1) "],
    ("RETURNING " <> intercalate ", " (map (pgFmtColumn qi) returnings)) `emptyOnFalse` null returnings
    ]
  where
    qi = QualifiedIdentifier schema mainTbl
    cols = intercalate ", " $ pgFmtIdent <$> S.toList keys
    payloadIsEmpty = pjIsEmpty p
requestToQuery schema _ (DbMutate (Update mainTbl p@(PayloadJSON _ pType keys) logicForest returnings)) =
  if pjIsEmpty p
    then "WITH " <> ignoredBody <> "SELECT ''"
    else
      unwords [
        "UPDATE " <> fromQi qi <> " SET " <> cols,
        "FROM (SELECT * FROM ",
        case pType of
           PJObject  -> " json_populate_record"
           PJArray _ -> " json_populate_recordset", "(null::", fromQi qi, ", $1)) _ ",
        ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest,
        ("RETURNING " <> intercalate ", " (map (pgFmtColumn qi) returnings)) `emptyOnFalse` null returnings
        ]
  where
    qi = QualifiedIdentifier schema mainTbl
    cols = intercalate ", " (pgFmtIdent <> const " = _." <> pgFmtIdent <$> S.toList keys)
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
limitF r  = if r == allRange
  then ""
  else "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit  = maybe "ALL" show $ rangeLimit r
    offset = show $ rangeOffset r

fromQi :: QualifiedIdentifier -> SqlFragment
fromQi t = (if s == "" then "" else pgFmtIdent s <> ".") <> pgFmtIdent n
  where
    n = qiName t
    s = qiSchema t

unicodeStatement :: Text -> HE.Params a -> HD.Result b -> Bool -> H.Query a b
unicodeStatement = H.statement . T.encodeUtf8

emptyOnFalse :: Text -> Bool -> Text
emptyOnFalse val cond = if cond then "" else val

pgFmtColumn :: QualifiedIdentifier -> Text -> SqlFragment
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c = fromQi table <> "." <> pgFmtIdent c

pgFmtField :: QualifiedIdentifier -> Field -> SqlFragment
pgFmtField table (c, jp) = pgFmtColumn table c <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> SqlFragment
pgFmtSelectItem table (f@(_, jp), Nothing, alias, _) = pgFmtField table f <> pgFmtAs jp alias
pgFmtSelectItem table (f@(_, jp), Just cast, alias, _) = "CAST (" <> pgFmtField table f <> " AS " <> cast <> " )" <> pgFmtAs jp alias

pgFmtFilter :: QualifiedIdentifier -> Filter -> SqlFragment
pgFmtFilter table (Filter fld (OpExpr hasNot oper)) = notOp <> " " <> case oper of
   Op op val  -> pgFmtFieldOp op <> " " <> case op of
     "like"   -> unknownLiteral (T.map star val)
     "ilike"  -> unknownLiteral (T.map star val)
     "is"     -> whiteList val
     _        -> unknownLiteral val

   In vals -> pgFmtField table fld <> " " <>
    let emptyValForIn = "= any('{}') " in -- Workaround because for postgresql "col IN ()" is invalid syntax, we instead do "col = any('{}')"
    case ((&&) (length vals == 1) . T.null) <$> headMay vals of
      Just False -> sqlOperator "in" <> "(" <> intercalate ", " (map unknownLiteral vals) <> ") "
      Just True  -> emptyValForIn
      Nothing    -> emptyValForIn

   Fts op lang val ->
     pgFmtFieldOp op
       <> "("
       <> maybe "" ((<> ", ") . pgFmtLit) lang
       <> unknownLiteral val
       <> ") "

   Join fQi (ForeignKey Column{colTable=Table{tableName=fTableName}, colName=fColName}) ->
     pgFmtField fQi fld <> " = " <> pgFmtColumn (removeSourceCTESchema (qiSchema fQi) fTableName) fColName
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

pgFmtLogicTree :: QualifiedIdentifier -> LogicTree -> SqlFragment
pgFmtLogicTree qi (Expr hasNot op forest) = notOp <> " (" <> intercalate (" " <> show op <> " ") (pgFmtLogicTree qi <$> forest) <> ")"
  where notOp =  if hasNot then "NOT" else ""
pgFmtLogicTree qi (Stmnt flt) = pgFmtFilter qi flt

pgFmtJsonPath :: Maybe JsonPath -> SqlFragment
pgFmtJsonPath (Just [x]) = "->>" <> pgFmtLit x
pgFmtJsonPath (Just (x:xs)) = "->" <> pgFmtLit x <> pgFmtJsonPath ( Just xs )
pgFmtJsonPath _ = ""

pgFmtAs :: Maybe JsonPath -> Maybe Alias -> SqlFragment
pgFmtAs Nothing Nothing = ""
pgFmtAs (Just xx) Nothing = case lastMay xx of
  Just alias -> " AS " <> pgFmtIdent alias
  Nothing -> ""
pgFmtAs _ (Just alias) = " AS " <> pgFmtIdent alias

pgFmtEnvVar :: Text -> (Text, Text) -> SqlFragment
pgFmtEnvVar prefix (k, v) =
  "set local " <> pgFmtIdent (prefix <> k) <> " = " <> pgFmtLit v <> ";"

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')
