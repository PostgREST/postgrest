{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
{-|
Module      : PostgREST.Query.SqlFragment
Description : Helper functions for PostgREST.QueryBuilder.

Any function that outputs a SqlFragment should be in this module.
-}
module PostgREST.Query.SqlFragment
  ( noLocationF
  , SqlFragment
  , asBinaryF
  , asCsvF
  , asGeoJsonF
  , asJsonF
  , asJsonNullF
  , asJsonSingleF
  , asXmlF
  , countF
  , fromQi
  , limitOffsetF
  , locationF
  , mutRangeF
  , normalizedBody
  , orderF
  , pgFmtColumn
  , pgFmtIdent
  , pgFmtIdentList
  , pgFmtJoinCondition
  , pgFmtLogicTree
  , pgFmtOrderTerm
  , pgFmtSelectItem
  , pgFmtSelectFromJson
  , responseHeadersF
  , responseStatusF
  , returningF
  , selectBody
  , singleParameter
  , sourceCTEName
  , unknownEncoder
  , intercalateSnippet
  , explainF
  , setConfigLocal
  , setConfigLocalJson
  ) where

import qualified Data.Aeson                      as JSON
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Hasql.DynamicStatements.Snippet as SQL
import qualified Hasql.Encoders                  as HE

import Control.Arrow ((***))

import Data.Foldable                 (foldr1)
import Text.InterpolatedString.Perl6 (qc)

import PostgREST.ApiRequest.Types        (Alias, Cast, Field,
                                          Filter (..),
                                          FtsOperator (..),
                                          JsonOperand (..),
                                          JsonOperation (..),
                                          JsonPath,
                                          LogicOperator (..),
                                          LogicTree (..), OpExpr (..),
                                          Operation (..),
                                          OrderDirection (..),
                                          OrderNulls (..),
                                          OrderTerm (..),
                                          SimpleOperator (..),
                                          TrileanVal (..))
import PostgREST.MediaType               (MTPlanFormat (..),
                                          MTPlanOption (..))
import PostgREST.Plan.ReadPlan           (JoinCondition (..))
import PostgREST.Plan.Types              (TypedField (..))
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          rangeLimit, rangeOffset)
import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..))

import Protolude hiding (cast)


-- | A part of a SQL query that cannot be executed independently
type SqlFragment = ByteString

noLocationF :: SqlFragment
noLocationF = "array[]::text[]"

sourceCTEName :: SqlFragment
sourceCTEName = "pgrst_source"

singleValOperator :: SimpleOperator -> SqlFragment
singleValOperator = \case
  OpEqual            -> "="
  OpGreaterThanEqual -> ">="
  OpGreaterThan      -> ">"
  OpLessThanEqual    -> "<="
  OpLessThan         -> "<"
  OpNotEqual         -> "<>"
  OpLike             -> "like"
  OpILike            -> "ilike"
  OpContains         -> "@>"
  OpContained        -> "<@"
  OpOverlap          -> "&&"
  OpStrictlyLeft     -> "<<"
  OpStrictlyRight    -> ">>"
  OpNotExtendsRight  -> "&<"
  OpNotExtendsLeft   -> "&>"
  OpAdjacent         -> "-|-"
  OpMatch            -> "~"
  OpIMatch           -> "~*"

ftsOperator :: FtsOperator -> SqlFragment
ftsOperator = \case
  FilterFts          -> "@@ to_tsquery"
  FilterFtsPlain     -> "@@ plainto_tsquery"
  FilterFtsPhrase    -> "@@ phraseto_tsquery"
  FilterFtsWebsearch -> "@@ websearch_to_tsquery"

-- |
-- These CTEs convert a json object into a json array, this way we can use json_to_recordset for all json payloads
-- Otherwise we'd have to use json_to_record for json objects and json_to_recordset for json arrays
-- We do this in SQL to avoid processing the JSON in application code
-- TODO: At this stage there shouldn't be a Maybe since ApiRequest should ensure that an INSERT/UPDATE has a body
normalizedBody :: Maybe LBS.ByteString -> SQL.Snippet
normalizedBody body =
  "pgrst_payload AS (SELECT " <> jsonPlaceHolder <> " AS json_data), " <>
  SQL.sql (BS.unwords [
    "pgrst_body AS (",
      "SELECT",
        "CASE WHEN json_typeof(json_data) = 'array'",
          "THEN json_data",
          "ELSE json_build_array(json_data)",
        "END AS val",
      "FROM pgrst_payload)"])
  where
    jsonPlaceHolder = SQL.encoderAndParam (HE.nullable HE.jsonLazyBytes) body

singleParameter :: Maybe LBS.ByteString -> ByteString -> SQL.Snippet
singleParameter body typ =
  if typ == "bytea"
    -- TODO: Hasql fails when using HE.unknown with bytea(pg tries to utf8 encode).
    then SQL.encoderAndParam (HE.nullable HE.bytea) (LBS.toStrict <$> body)
    else SQL.encoderAndParam (HE.nullable HE.unknown) (LBS.toStrict <$> body) <> "::" <> SQL.sql typ

selectBody :: SqlFragment
selectBody = "(SELECT val FROM pgrst_body)"

-- Here we build the pg array literal, e.g '{"Hebdon, John","Other","Another"}', manually.
-- This is necessary to pass an "unknown" array and let pg infer the type.
-- There are backslashes here, but since this value is parametrized and is not a string constant
-- https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-STRINGS
-- we don't need to use the E'string' form for C-style escapes
-- https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-STRINGS-ESCAPE
pgBuildArrayLiteral :: [Text] -> Text
pgBuildArrayLiteral vals =
 let trimmed = trimNullChars
     slashed = T.replace "\\" "\\\\" . trimmed
     escaped x = "\"" <> T.replace "\"" "\\\"" (slashed x) <> "\"" in
 "{" <> T.intercalate "," (escaped <$> vals) <> "}"

-- TODO: refactor by following https://github.com/PostgREST/postgrest/pull/1631#issuecomment-711070833
pgFmtIdent :: Text -> SqlFragment
pgFmtIdent x = encodeUtf8 $ "\"" <> T.replace "\"" "\"\"" (trimNullChars x) <> "\""

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')

-- |
-- Format a list of identifiers and separate them by commas.
--
-- >>> pgFmtIdentList ["schema_1", "schema_2", "SPECIAL \"@/\\#~_-"]
-- "\"schema_1\", \"schema_2\", \"SPECIAL \"\"@/\\#~_-\""
pgFmtIdentList :: [Text] -> SqlFragment
pgFmtIdentList schemas = BS.intercalate ", " $ pgFmtIdent <$> schemas

asCsvF :: SqlFragment
asCsvF = asCsvHeaderF <> " || '\n' || " <> asCsvBodyF
  where
    asCsvHeaderF =
      "(SELECT coalesce(string_agg(a.k, ','), '')" <>
      "  FROM (" <>
      "    SELECT json_object_keys(r)::text as k" <>
      "    FROM ( " <>
      "      SELECT row_to_json(hh) as r from " <> sourceCTEName <> " as hh limit 1" <>
      "    ) s" <>
      "  ) a" <>
      ")"
    asCsvBodyF = "coalesce(string_agg(substring(_postgrest_t::text, 2, length(_postgrest_t::text) - 2), '\n'), '')"

asJsonF :: Bool -> SqlFragment
asJsonF returnsScalar
  | returnsScalar = "coalesce(json_agg(_postgrest_t.pgrst_scalar), '[]')::character varying"
  | otherwise     = "coalesce(json_agg(_postgrest_t), '[]')::character varying"

asJsonNullF :: Bool -> SqlFragment
asJsonNullF returnsScalar
  | returnsScalar = "coalesce(json_strip_nulls (json_agg(_postgrest_t.pgrst_scalar)), '[]')::character varying"
  | otherwise     = "coalesce(json_strip_nulls (json_agg(_postgrest_t)), '[]')::character varying"

asJsonSingleF :: Bool -> SqlFragment
asJsonSingleF returnsScalar
  | returnsScalar = "coalesce((json_agg(_postgrest_t.pgrst_scalar)->0)::text, 'null')"
  | otherwise     = "coalesce((json_agg(_postgrest_t)->0)::text, 'null')"

asXmlF :: FieldName -> SqlFragment
asXmlF fieldName = "coalesce(xmlagg(_postgrest_t." <> pgFmtIdent fieldName <> "), '')"

asGeoJsonF ::  SqlFragment
asGeoJsonF = "json_build_object('type', 'FeatureCollection', 'features', coalesce(json_agg(ST_AsGeoJSON(_postgrest_t)::json), '[]'))"

asBinaryF :: FieldName -> SqlFragment
asBinaryF fieldName = "coalesce(string_agg(_postgrest_t." <> pgFmtIdent fieldName <> ", ''), '')"

locationF :: [Text] -> SqlFragment
locationF pKeys = [qc|(
  WITH data AS (SELECT row_to_json(_) AS row FROM {sourceCTEName} AS _ LIMIT 1)
  SELECT array_agg(json_data.key || '=' || coalesce('eq.' || json_data.value, 'is.null'))
  FROM data CROSS JOIN json_each_text(data.row) AS json_data
  WHERE json_data.key IN ('{fmtPKeys}')
)|]
  where
    fmtPKeys = T.intercalate "','" pKeys

fromQi :: QualifiedIdentifier -> SqlFragment
fromQi t = (if T.null s then mempty else pgFmtIdent s <> ".") <> pgFmtIdent n
  where
    n = qiName t
    s = qiSchema t

pgFmtColumn :: QualifiedIdentifier -> Text -> SqlFragment
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c   = fromQi table <> "." <> pgFmtIdent c

pgFmtField :: QualifiedIdentifier -> Field -> SQL.Snippet
pgFmtField table (c, []) = SQL.sql (pgFmtColumn table c)
-- Using to_jsonb instead of to_json to avoid missing operator errors when filtering:
-- "operator does not exist: json = unknown"
pgFmtField table (c, jp) = SQL.sql ("to_jsonb(" <> pgFmtColumn table c <> ")") <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> (Field, Maybe Cast, Maybe Alias) -> SQL.Snippet
pgFmtSelectItem table (f@(fName, jp), Nothing, alias) = pgFmtField table f <> SQL.sql (pgFmtAs fName jp alias)
-- Ideally we'd quote the cast with "pgFmtIdent cast". However, that would invalidate common casts such as "int", "bigint", etc.
-- Try doing: `select 1::"bigint"` - it'll err, using "int8" will work though. There's some parser magic that pg does that's invalidated when quoting.
-- Not quoting should be fine, we validate the input on Parsers.
pgFmtSelectItem table (f@(fName, jp), Just cast, alias) = "CAST (" <> pgFmtField table f <> " AS " <> SQL.sql (encodeUtf8 cast) <> " )" <> SQL.sql (pgFmtAs fName jp alias)

pgFmtSelectFromJson :: [TypedField] -> SQL.Snippet
pgFmtSelectFromJson fields =
  SQL.sql "SELECT " <> parsedCols <> " " <>
  (if null fields
    -- When we are inserting no columns (e.g. using default values), we can't use our ordinary `json_to_recordset`
    -- because it can't extract records with no columns (there's no valid syntax for the `AS (colName colType,...)`
    -- part). But we still need to ensure as many rows are created as there are array elements.
    then SQL.sql ("FROM json_array_elements (" <> selectBody <> ") _ ")
    else SQL.sql ("FROM json_to_recordset (" <> selectBody <> ") AS _ " <> "(" <> typedCols <> ") ")
  )
  where
    parsedCols = SQL.sql $ BS.intercalate ", " $ pgFmtIdent . tfName <$> fields
    typedCols = BS.intercalate ", " $ pgFmtIdent . tfName <> const " " <> encodeUtf8 . tfIRType <$> fields

pgFmtOrderTerm :: QualifiedIdentifier -> OrderTerm -> SQL.Snippet
pgFmtOrderTerm qi ot =
  fmtOTerm ot <> " " <>
  SQL.sql (BS.unwords [
    maybe mempty direction $ otDirection ot,
    maybe mempty nullOrder $ otNullOrder ot])
  where
    fmtOTerm = \case
      OrderTerm{otTerm}                        -> pgFmtField qi otTerm
      OrderRelationTerm{otRelation, otRelTerm} -> pgFmtField (QualifiedIdentifier mempty otRelation) otRelTerm

    direction OrderAsc  = "ASC"
    direction OrderDesc = "DESC"

    nullOrder OrderNullsFirst = "NULLS FIRST"
    nullOrder OrderNullsLast  = "NULLS LAST"


pgFmtFilter :: QualifiedIdentifier -> Filter -> SQL.Snippet
pgFmtFilter _ (FilterNullEmbed hasNot fld) = SQL.sql (pgFmtIdent fld) <> " IS " <> (if hasNot then "NOT" else mempty) <> " NULL"
pgFmtFilter _ (Filter _ (NoOpExpr _)) = mempty -- TODO unreachable because NoOpExpr is filtered on QueryParams
pgFmtFilter table (Filter fld (OpExpr hasNot oper)) = notOp <> " " <> case oper of
   Op op val  -> pgFmtFieldOp op <> " " <> case op of
     OpLike  -> unknownLiteral (T.map star val)
     OpILike -> unknownLiteral (T.map star val)
     _       -> unknownLiteral val

   -- IS cannot be prepared. `PREPARE boolplan AS SELECT * FROM projects where id IS $1` will give a syntax error.
   -- The above can be fixed by using `PREPARE boolplan AS SELECT * FROM projects where id IS NOT DISTINCT FROM $1;`
   -- However that would not accept the TRUE/FALSE/NULL/UNKNOWN keywords. See: https://stackoverflow.com/questions/6133525/proper-way-to-set-preparedstatement-parameter-to-null-under-postgres.
   -- This is why `IS` operands are whitelisted at the Parsers.hs level
   Is triVal -> pgFmtField table fld <> " IS " <> case triVal of
     TriTrue    -> "TRUE"
     TriFalse   -> "FALSE"
     TriNull    -> "NULL"
     TriUnknown -> "UNKNOWN"

   -- We don't use "IN", we use "= ANY". IN has the following disadvantages:
   -- + No way to use an empty value on IN: "col IN ()" is invalid syntax. With ANY we can do "= ANY('{}')"
   -- + Can invalidate prepared statements: multiple parameters on an IN($1, $2, $3) will lead to using different prepared statements and not take advantage of caching.
   In vals -> pgFmtField table fld <> " " <> case vals of
      [""] -> "= ANY('{}') "
      _    -> "= ANY (" <> unknownLiteral (pgBuildArrayLiteral vals) <> ") "

   Fts op lang val ->
     pgFmtFieldFts op <> "(" <> ftsLang lang <> unknownLiteral val <> ") "
 where
   ftsLang = maybe mempty (\l -> unknownLiteral l <> ", ")
   pgFmtFieldOp op = pgFmtField table fld <> " " <> SQL.sql (singleValOperator op)
   pgFmtFieldFts op = pgFmtField table fld <> " " <> SQL.sql (ftsOperator op)
   notOp = if hasNot then "NOT" else mempty
   star c = if c == '*' then '%' else c

pgFmtJoinCondition :: JoinCondition -> SQL.Snippet
pgFmtJoinCondition (JoinCondition (qi1, col1) (qi2, col2)) =
  SQL.sql $ pgFmtColumn qi1 col1 <> " = " <> pgFmtColumn qi2 col2

pgFmtLogicTree :: QualifiedIdentifier -> LogicTree -> SQL.Snippet
pgFmtLogicTree qi (Expr hasNot op forest) = SQL.sql notOp <> " (" <> intercalateSnippet (opSql op) (pgFmtLogicTree qi <$> forest) <> ")"
  where
    notOp =  if hasNot then "NOT" else mempty

    opSql And = " AND "
    opSql Or  = " OR "
pgFmtLogicTree qi (Stmnt flt) = pgFmtFilter qi flt

pgFmtJsonPath :: JsonPath -> SQL.Snippet
pgFmtJsonPath = \case
  []             -> mempty
  (JArrow x:xs)  -> "->" <> pgFmtJsonOperand x <> pgFmtJsonPath xs
  (J2Arrow x:xs) -> "->>" <> pgFmtJsonOperand x <> pgFmtJsonPath xs
  where
    pgFmtJsonOperand (JKey k) = unknownLiteral k
    pgFmtJsonOperand (JIdx i) = unknownLiteral i <> "::int"

pgFmtAs :: FieldName -> JsonPath -> Maybe Alias -> SqlFragment
pgFmtAs _ [] Nothing = mempty
pgFmtAs fName jp Nothing = case jOp <$> lastMay jp of
  Just (JKey key) -> " AS " <> pgFmtIdent key
  Just (JIdx _)   -> " AS " <> pgFmtIdent (fromMaybe fName lastKey)
    -- We get the lastKey because on:
    -- `select=data->1->mycol->>2`, we need to show the result as [ {"mycol": ..}, {"mycol": ..} ]
    -- `select=data->3`, we need to show the result as [ {"data": ..}, {"data": ..} ]
    where lastKey = jVal <$> find (\case JKey{} -> True; _ -> False) (jOp <$> reverse jp)
  Nothing -> mempty
pgFmtAs _ _ (Just alias) = " AS " <> pgFmtIdent alias

countF :: SQL.Snippet -> Bool -> (SQL.Snippet, SqlFragment)
countF countQuery shouldCount =
  if shouldCount
    then (
        ", pgrst_source_count AS (" <> countQuery <> ")"
      , "(SELECT pg_catalog.count(*) FROM pgrst_source_count)" )
    else (
        mempty
      , "null::bigint")

returningF :: QualifiedIdentifier -> [FieldName] -> SqlFragment
returningF qi returnings =
  if null returnings
    then "RETURNING 1" -- For mutation cases where there's no ?select, we return 1 to know how many rows were modified
    else "RETURNING " <> BS.intercalate ", " (pgFmtColumn qi <$> returnings)

limitOffsetF :: NonnegRange -> SQL.Snippet
limitOffsetF range =
  if range == allRange then mempty else "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit = maybe "ALL" (\l -> unknownEncoder (BS.pack $ show l)) $ rangeLimit range
    offset = unknownEncoder (BS.pack . show $ rangeOffset range)

responseHeadersF :: SqlFragment
responseHeadersF = currentSettingF "response.headers"

responseStatusF :: SqlFragment
responseStatusF = currentSettingF "response.status"

currentSettingF :: SqlFragment -> SqlFragment
currentSettingF setting =
  -- nullif is used because of https://gist.github.com/steve-chavez/8d7033ea5655096903f3b52f8ed09a15
  "nullif(current_setting('" <> setting <> "', true), '')"

mutRangeF :: QualifiedIdentifier -> [FieldName] -> (SqlFragment, SqlFragment)
mutRangeF mainQi rangeId =
  (
    BS.intercalate " AND " $ (\col -> pgFmtColumn mainQi col <> " = " <> pgFmtColumn (QualifiedIdentifier mempty "pgrst_affected_rows") col) <$> rangeId
  , BS.intercalate ", " (pgFmtColumn mainQi <$> rangeId)
  )

orderF :: QualifiedIdentifier -> [OrderTerm] -> SQL.Snippet
orderF _ []    = mempty
orderF qi ordts = "ORDER BY " <> intercalateSnippet ", " (pgFmtOrderTerm qi <$> ordts)

-- Hasql Snippet utilities
unknownEncoder :: ByteString -> SQL.Snippet
unknownEncoder = SQL.encoderAndParam (HE.nonNullable HE.unknown)

unknownLiteral :: Text -> SQL.Snippet
unknownLiteral = unknownEncoder . encodeUtf8

intercalateSnippet :: ByteString -> [SQL.Snippet] -> SQL.Snippet
intercalateSnippet _ [] = mempty
intercalateSnippet frag snippets = foldr1 (\a b -> a <> SQL.sql frag <> b) snippets

explainF :: MTPlanFormat -> [MTPlanOption] -> SQL.Snippet -> SQL.Snippet
explainF fmt opts snip =
  "EXPLAIN (" <>
    SQL.sql (BS.intercalate ", " (fmtPlanFmt fmt : (fmtPlanOpt <$> opts))) <>
  ") " <> snip
  where
    fmtPlanOpt :: MTPlanOption -> BS.ByteString
    fmtPlanOpt PlanAnalyze  = "ANALYZE"
    fmtPlanOpt PlanVerbose  = "VERBOSE"
    fmtPlanOpt PlanSettings = "SETTINGS"
    fmtPlanOpt PlanBuffers  = "BUFFERS"
    fmtPlanOpt PlanWAL      = "WAL"

    fmtPlanFmt PlanJSON = "FORMAT JSON"
    fmtPlanFmt PlanText = "FORMAT TEXT"

-- | Do a pg set_config(setting, value, true) call. This is equivalent to a SET LOCAL.
setConfigLocal :: ByteString -> (ByteString, ByteString) -> SQL.Snippet
setConfigLocal prefix (k, v) =
  "set_config(" <> unknownEncoder (prefix <> k) <> ", " <> unknownEncoder v <> ", true)"

-- | Starting from PostgreSQL v14, some characters are not allowed for config names (mostly affecting headers with "-").
-- | A JSON format string is used to avoid this problem. See https://github.com/PostgREST/postgrest/issues/1857
setConfigLocalJson :: ByteString -> [(ByteString, ByteString)] -> [SQL.Snippet]
setConfigLocalJson prefix keyVals = [setConfigLocal mempty (prefix, gucJsonVal keyVals)]
  where
    gucJsonVal :: [(ByteString, ByteString)] -> ByteString
    gucJsonVal = LBS.toStrict . JSON.encode . HM.fromList . arrayByteStringToText
    arrayByteStringToText :: [(ByteString, ByteString)] -> [(Text,Text)]
    arrayByteStringToText keyVal = (T.decodeUtf8 *** T.decodeUtf8) <$> keyVal
