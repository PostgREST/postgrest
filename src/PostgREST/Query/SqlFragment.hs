{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
{-|
Module      : PostgREST.Query.SqlFragment
Description : Helper functions for PostgREST.QueryBuilder.
-}
module PostgREST.Query.SqlFragment
  ( noLocationF
  , aggF
  , countF
  , groupF
  , fromQi
  , limitOffsetF
  , locationF
  , mutRangeF
  , orderF
  , pgFmtColumn
  , pgFmtIdent
  , pgFmtJoinCondition
  , pgFmtLogicTree
  , pgFmtOrderTerm
  , pgFmtSelectItem
  , fromJsonBodyF
  , responseHeadersF
  , responseStatusF
  , returningF
  , singleParameter
  , sourceCTE
  , sourceCTEName
  , unknownEncoder
  , intercalateSnippet
  , explainF
  , setConfigLocal
  , setConfigLocalJson
  , escapeIdent
  , escapeIdentList
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

import PostgREST.ApiRequest.Types        (AggregateFunction (..),
                                          Alias,
                                          FtsOperator (..),
                                          JsonOperand (..),
                                          JsonOperation (..),
                                          JsonPath,
                                          LogicOperator (..),
                                          OpExpr (..),
                                          OpQuantifier (..),
                                          Operation (..),
                                          OrderDirection (..),
                                          OrderNulls (..),
                                          QuantOperator (..),
                                          SimpleOperator (..),
                                          TrileanVal (..))
import PostgREST.MediaType               (MTPlanFormat (..),
                                          MTPlanOption (..))
import PostgREST.Plan.ReadPlan           (JoinCondition (..))
import PostgREST.Plan.Types              (CoercibleField (..),
                                          CoercibleFilter (..),
                                          CoercibleLogicTree (..),
                                          CoercibleOrderTerm (..),
                                          SelectTerm(..),
                                          unknownField)
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          rangeLimit, rangeOffset)
import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..))
import PostgREST.SchemaCache.Routine     (ResultAggregate (..),
                                          Routine (..),
                                          funcReturnsScalar,
                                          funcReturnsSetOfScalar,
                                          funcReturnsSingleComposite)

import Protolude hiding (cast, Sum)

sourceCTEName :: Text
sourceCTEName = "pgrst_source"

sourceCTE :: SQL.Snippet
sourceCTE = "pgrst_source"

noLocationF :: SQL.Snippet
noLocationF = "array[]::text[]"

simpleOperator :: SimpleOperator -> SQL.Snippet
simpleOperator = \case
  OpNotEqual        -> "<>"
  OpContains        -> "@>"
  OpContained       -> "<@"
  OpOverlap         -> "&&"
  OpStrictlyLeft    -> "<<"
  OpStrictlyRight   -> ">>"
  OpNotExtendsRight -> "&<"
  OpNotExtendsLeft  -> "&>"
  OpAdjacent        -> "-|-"

quantOperator :: QuantOperator -> SQL.Snippet
quantOperator = \case
  OpEqual            -> "="
  OpGreaterThanEqual -> ">="
  OpGreaterThan      -> ">"
  OpLessThanEqual    -> "<="
  OpLessThan         -> "<"
  OpLike             -> "like"
  OpILike            -> "ilike"
  OpMatch            -> "~"
  OpIMatch           -> "~*"

ftsOperator :: FtsOperator -> SQL.Snippet
ftsOperator = \case
  FilterFts          -> "@@ to_tsquery"
  FilterFtsPlain     -> "@@ plainto_tsquery"
  FilterFtsPhrase    -> "@@ phraseto_tsquery"
  FilterFtsWebsearch -> "@@ websearch_to_tsquery"

singleParameter :: Maybe LBS.ByteString -> ByteString -> SQL.Snippet
singleParameter body typ =
  if typ == "bytea"
    -- TODO: Hasql fails when using HE.unknown with bytea(pg tries to utf8 encode).
    then SQL.encoderAndParam (HE.nullable HE.bytea) (LBS.toStrict <$> body)
    else SQL.encoderAndParam (HE.nullable HE.unknown) (LBS.toStrict <$> body) <> "::" <> SQL.sql typ

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
pgFmtIdent :: Text -> SQL.Snippet
pgFmtIdent x = SQL.sql $ escapeIdent x

escapeIdent :: Text -> ByteString
escapeIdent x = encodeUtf8 $ "\"" <> T.replace "\"" "\"\"" (trimNullChars x) <> "\""

-- Only use it if the input comes from the database itself, like on `jsonb_build_object('column_from_a_table', val)..`
pgFmtLit :: Text -> Text
pgFmtLit x =
  let trimmed = trimNullChars x
      escaped = "'" <> T.replace "'" "''" trimmed <> "'"
      slashed = T.replace "\\" "\\\\" escaped in
  if "\\" `T.isInfixOf` escaped
    then "E" <> slashed
    else slashed

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')

-- |
-- Format a list of identifiers and separate them by commas.
--
-- >>> escapeIdentList ["schema_1", "schema_2", "SPECIAL \"@/\\#~_-"]
-- "\"schema_1\", \"schema_2\", \"SPECIAL \"\"@/\\#~_-\""
escapeIdentList :: [Text] -> ByteString
escapeIdentList schemas = BS.intercalate ", " $ escapeIdent <$> schemas

asCsvF :: SQL.Snippet
asCsvF = asCsvHeaderF <> " || '\n' || " <> asCsvBodyF
  where
    asCsvHeaderF =
      "(SELECT coalesce(string_agg(a.k, ','), '')" <>
      "  FROM (" <>
      "    SELECT json_object_keys(r)::text as k" <>
      "    FROM ( " <>
      "      SELECT row_to_json(hh) as r from " <> sourceCTE <> " as hh limit 1" <>
      "    ) s" <>
      "  ) a" <>
      ")"
    asCsvBodyF = "coalesce(string_agg(substring(_postgrest_t::text, 2, length(_postgrest_t::text) - 2), '\n'), '')"

addNullsToSnip :: Bool -> SQL.Snippet -> SQL.Snippet
addNullsToSnip strip snip =
  if strip then "json_strip_nulls(" <> snip <> ")" else  snip

asJsonSingleF :: Maybe Routine -> Bool -> SQL.Snippet
asJsonSingleF rout strip
  | returnsScalar = "coalesce(" <> addNullsToSnip strip "json_agg(_postgrest_t.pgrst_scalar)->0"  <> ", 'null')"
  | otherwise     = "coalesce(" <> addNullsToSnip strip "json_agg(_postgrest_t)->0"  <> ", 'null')"
  where
    returnsScalar = maybe False funcReturnsScalar rout

asJsonF :: Maybe Routine -> Bool -> SQL.Snippet
asJsonF rout strip
  | returnsSingleComposite    = "coalesce(" <> addNullsToSnip strip "json_agg(_postgrest_t)->0" <> ", 'null')"
  | returnsScalar             = "coalesce(" <> addNullsToSnip strip "json_agg(_postgrest_t.pgrst_scalar)->0" <> ", 'null')"
  | returnsSetOfScalar        = "coalesce(" <> addNullsToSnip strip "json_agg(_postgrest_t.pgrst_scalar)" <> ", '[]')"
  | otherwise                 = "coalesce(" <> addNullsToSnip strip "json_agg(_postgrest_t)" <> ", '[]')"
  where
    (returnsSingleComposite, returnsScalar, returnsSetOfScalar) = case rout of
      Just r  -> (funcReturnsSingleComposite r, funcReturnsScalar r, funcReturnsSetOfScalar r)
      Nothing -> (False, False, False)


asXmlF :: Maybe FieldName -> SQL.Snippet
asXmlF (Just fieldName) = "coalesce(xmlagg(_postgrest_t." <> pgFmtIdent fieldName <> "), '')"
-- TODO unreachable because a previous step(binaryField) will validate that there's a field. This will be cleared once custom media types are implemented.
asXmlF Nothing          = "coalesce(xmlagg(_postgrest_t), '')"

asGeoJsonF ::  SQL.Snippet
asGeoJsonF = "json_build_object('type', 'FeatureCollection', 'features', coalesce(json_agg(ST_AsGeoJSON(_postgrest_t)::json), '[]'))"

asBinaryF :: Maybe FieldName -> SQL.Snippet
asBinaryF (Just fieldName) = "coalesce(string_agg(_postgrest_t." <> pgFmtIdent fieldName <> ", ''), '')"
-- TODO unreachable because a previous step(binaryField) will validate that there's a field. This will be cleared once custom media types are implemented.
asBinaryF Nothing          = "coalesce(string_agg(_postgrest_t, ''), '')"

locationF :: [Text] -> SQL.Snippet
locationF pKeys = [qc|(
  WITH data AS (SELECT row_to_json(_) AS row FROM {sourceCTEName} AS _ LIMIT 1)
  SELECT array_agg(json_data.key || '=' || coalesce('eq.' || json_data.value, 'is.null'))
  FROM data CROSS JOIN json_each_text(data.row) AS json_data
  WHERE json_data.key IN ('{fmtPKeys}')
)|]
  where
    fmtPKeys = T.intercalate "','" pKeys

fromQi :: QualifiedIdentifier -> SQL.Snippet
fromQi t = (if T.null s then mempty else pgFmtIdent s <> ".") <> pgFmtIdent n
  where
    n = qiName t
    s = qiSchema t

pgFmtColumn :: QualifiedIdentifier -> Text -> SQL.Snippet
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c   = fromQi table <> "." <> pgFmtIdent c

pgFmtCallUnary :: Text -> SQL.Snippet -> SQL.Snippet
pgFmtCallUnary f x = SQL.sql (encodeUtf8 f) <> "(" <> x <> ")"

pgFmtField :: QualifiedIdentifier -> CoercibleField -> SQL.Snippet
pgFmtField table CoercibleField{cfName=fn, cfJsonPath=[]}                                = pgFmtColumn table fn
pgFmtField table CoercibleField{cfName=fn, cfToJson=doToJson, cfJsonPath=jp} | doToJson  = "to_jsonb(" <> pgFmtColumn table fn <> ")" <> pgFmtJsonPath jp
                                                                             | otherwise = pgFmtColumn table fn <> pgFmtJsonPath jp

-- Select the value of a named element from a table, applying its optional coercion mapping if any.
pgFmtTableCoerce :: QualifiedIdentifier -> CoercibleField -> SQL.Snippet
pgFmtTableCoerce table fld@(CoercibleField{cfTransform=(Just formatterProc)}) = pgFmtCallUnary formatterProc (pgFmtField table fld)
pgFmtTableCoerce table f = pgFmtField table f

-- | Like the previous but now we just have a name so no namespace or JSON paths.
pgFmtCoerceNamed :: CoercibleField -> SQL.Snippet
pgFmtCoerceNamed CoercibleField{cfName=fn, cfTransform=(Just formatterProc)} = pgFmtCallUnary formatterProc (pgFmtIdent fn) <> " AS " <> pgFmtIdent fn
pgFmtCoerceNamed CoercibleField{cfName=fn} = pgFmtIdent fn

pgFmtSelectItem :: QualifiedIdentifier -> SelectTerm -> SQL.Snippet
pgFmtSelectItem table SelectTerm{selField=fld, selAggFunction=agg, selCast=Nothing, selAlias=alias} =
  pgFmtApplyAggregate agg (pgFmtTableCoerce table fld) <> pgFmtAs (cfName fld) (cfJsonPath fld) alias
pgFmtSelectItem table SelectTerm{selField=fld, selAggFunction=agg, selCast=(Just cast), selAlias=alias} =
  -- Ideally we'd quote the cast with "pgFmtIdent cast". However, that would invalidate common casts such as "int", "bigint", etc.
  -- Try doing: `select 1::"bigint"` - it'll err, using "int8" will work though. There's some parser magic that pg does that's invalidated when quoting.
  -- Not quoting should be fine, we validate the input on Parsers.
  pgFmtApplyAggregate agg ("CAST (" <> pgFmtTableCoerce table fld <> " AS " <> SQL.sql (encodeUtf8 cast) <> " )") <> pgFmtAs (cfName fld) (cfJsonPath fld) alias

pgFmtApplyAggregate :: Maybe AggregateFunction -> SQL.Snippet -> SQL.Snippet
pgFmtApplyAggregate Nothing snippet = snippet
pgFmtApplyAggregate (Just agg) snippet = case agg of
  Sum   -> "SUM( "   <> snippet <> " )"
  Max   -> "MAX( "   <> snippet <> " )"
  Min   -> "MIN( "   <> snippet <> " )"
  Avg   -> "AVG( "   <> snippet <> " )"
  Count -> "COUNT( " <> snippet <> " )"

-- TODO: At this stage there shouldn't be a Maybe since ApiRequest should ensure that an INSERT/UPDATE has a body
fromJsonBodyF :: Maybe LBS.ByteString -> [CoercibleField] -> Bool -> Bool -> Bool -> SQL.Snippet
fromJsonBodyF body fields includeSelect includeLimitOne includeDefaults =
  (if includeSelect then "SELECT " <> namedCols <> " " else mempty) <>
  "FROM (SELECT " <> jsonPlaceHolder <> " AS json_data) pgrst_payload, " <>
  -- convert a json object into a json array, this way we can use json_to_recordset for all json payloads
  -- Otherwise we'd have to use json_to_record for json objects and json_to_recordset for json arrays
  -- We do this in SQL to avoid processing the JSON in application code
  "LATERAL (SELECT CASE WHEN " <> jsonTypeofF <> "(pgrst_payload.json_data) = 'array' THEN pgrst_payload.json_data ELSE " <> jsonBuildArrayF <> "(pgrst_payload.json_data) END AS val) pgrst_uniform_json, " <>
  (if includeDefaults
    then "LATERAL (SELECT jsonb_agg(jsonb_build_object(" <> defsJsonb <> ") || elem) AS val from jsonb_array_elements(pgrst_uniform_json.val) elem) pgrst_json_defs, "
    else mempty) <>
  "LATERAL (SELECT " <> parsedCols <> " FROM " <>
    (if null fields
      -- When we are inserting no columns (e.g. using default values), we can't use our ordinary `json_to_recordset`
      -- because it can't extract records with no columns (there's no valid syntax for the `AS (colName colType,...)`
      -- part). But we still need to ensure as many rows are created as there are array elements.
      then SQL.sql $ jsonArrayElementsF <> "(" <> finalBodyF <> ") _ "
      else jsonToRecordsetF <> "(" <> SQL.sql finalBodyF <> ") AS _(" <> typedCols <> ") " <> if includeLimitOne then "LIMIT 1" else mempty
    ) <>
  ") pgrst_body "
  where
    namedCols = intercalateSnippet ", " $ fromQi  . QualifiedIdentifier "pgrst_body" . cfName <$> fields
    parsedCols = intercalateSnippet ", " $ pgFmtCoerceNamed <$> fields
    typedCols = intercalateSnippet ", " $ pgFmtIdent . cfName <> const " " <> SQL.sql . encodeUtf8 . cfIRType <$> fields
    defsJsonb = SQL.sql $ BS.intercalate "," fieldsWDefaults
    fieldsWDefaults = mapMaybe (\case
        CoercibleField{cfName=nam, cfDefault=Just def} -> Just $ encodeUtf8 (pgFmtLit nam <> ", " <> def)
        CoercibleField{cfDefault=Nothing} -> Nothing
      ) fields
    (finalBodyF, jsonTypeofF, jsonBuildArrayF, jsonArrayElementsF, jsonToRecordsetF) =
      if includeDefaults
        then ("pgrst_json_defs.val", "jsonb_typeof", "jsonb_build_array", "jsonb_array_elements", "jsonb_to_recordset")
        else ("pgrst_uniform_json.val", "json_typeof", "json_build_array", "json_array_elements", "json_to_recordset")
    jsonPlaceHolder = SQL.encoderAndParam (HE.nullable $ if includeDefaults then HE.jsonbLazyBytes else HE.jsonLazyBytes) body

pgFmtOrderTerm :: QualifiedIdentifier -> CoercibleOrderTerm -> SQL.Snippet
pgFmtOrderTerm qi ot =
  fmtOTerm ot <> " " <>
  SQL.sql (BS.unwords [
    maybe mempty direction $ coDirection ot,
    maybe mempty nullOrder $ coNullOrder ot])
  where
    fmtOTerm = \case
      CoercibleOrderTerm{coField=cof}                            -> pgFmtField qi cof
      CoercibleOrderRelationTerm{coRelation, coRelTerm=(fn, jp)} -> pgFmtField (QualifiedIdentifier mempty coRelation) (unknownField fn jp)

    direction OrderAsc  = "ASC"
    direction OrderDesc = "DESC"

    nullOrder OrderNullsFirst = "NULLS FIRST"
    nullOrder OrderNullsLast  = "NULLS LAST"

-- | Interpret a literal in the way the planner indicated through the CoercibleField.
pgFmtUnknownLiteralForField :: SQL.Snippet -> CoercibleField -> SQL.Snippet
pgFmtUnknownLiteralForField value CoercibleField{cfTransform=(Just parserProc)} = pgFmtCallUnary parserProc value
-- But when no transform is requested, we just use the literal as-is.
pgFmtUnknownLiteralForField value _ = value

-- | Array version of the above, used by ANY().
pgFmtArrayLiteralForField :: [Text] -> CoercibleField -> SQL.Snippet
-- When a transformation is requested, we need to apply the transformation to each element of the array. This could be done by just making a query with `parser(value)` for each value, but may lead to huge query lengths. Imagine `data_representations.color_from_text('...'::text)` for repeated for a hundred values. Instead we use `unnest()` to unpack a standard array literal and then apply the transformation to each element, like a map.
-- Note the literals will be treated as text since in every case when we use ANY() the parameters are textual (coming from a query string). We want to rely on the `text->domain` parser to do the right thing.
pgFmtArrayLiteralForField values CoercibleField{cfTransform=(Just parserProc)} = SQL.sql "(SELECT " <> pgFmtCallUnary parserProc (SQL.sql "unnest(" <> unknownLiteral (pgBuildArrayLiteral values) <> "::text[])") <> ")"
-- When no transformation is requested, we don't need a subquery.
pgFmtArrayLiteralForField values _ = unknownLiteral (pgBuildArrayLiteral values)


pgFmtFilter :: QualifiedIdentifier -> CoercibleFilter -> SQL.Snippet
pgFmtFilter _ (CoercibleFilterNullEmbed hasNot fld) = pgFmtIdent fld <> " IS " <> (if hasNot then "NOT" else mempty) <> " NULL"
pgFmtFilter _ (CoercibleFilter _ (NoOpExpr _)) = mempty -- TODO unreachable because NoOpExpr is filtered on QueryParams
pgFmtFilter table (CoercibleFilter fld (OpExpr hasNot oper)) = notOp <> " " <> pgFmtField table fld <> case oper of
   Op op val  -> " " <> simpleOperator op <> " " <> pgFmtUnknownLiteralForField (unknownLiteral val) fld

   OpQuant op quant val -> " " <> quantOperator op <> " " <> case op of
     OpLike  -> fmtQuant quant $ unknownLiteral (T.map star val)
     OpILike -> fmtQuant quant $ unknownLiteral (T.map star val)
     _       -> fmtQuant quant $ pgFmtUnknownLiteralForField (unknownLiteral val) fld

   -- IS cannot be prepared. `PREPARE boolplan AS SELECT * FROM projects where id IS $1` will give a syntax error.
   -- The above can be fixed by using `PREPARE boolplan AS SELECT * FROM projects where id IS NOT DISTINCT FROM $1;`
   -- However that would not accept the TRUE/FALSE/NULL/UNKNOWN keywords. See: https://stackoverflow.com/questions/6133525/proper-way-to-set-preparedstatement-parameter-to-null-under-postgres.
   -- This is why `IS` operands are whitelisted at the Parsers.hs level
   Is triVal -> " IS " <> case triVal of
     TriTrue    -> "TRUE"
     TriFalse   -> "FALSE"
     TriNull    -> "NULL"
     TriUnknown -> "UNKNOWN"

   IsDistinctFrom val -> " IS DISTINCT FROM " <> unknownLiteral val

   -- We don't use "IN", we use "= ANY". IN has the following disadvantages:
   -- + No way to use an empty value on IN: "col IN ()" is invalid syntax. With ANY we can do "= ANY('{}')"
   -- + Can invalidate prepared statements: multiple parameters on an IN($1, $2, $3) will lead to using different prepared statements and not take advantage of caching.
   In vals -> " " <> case vals of
      [""] -> "= ANY('{}') "
      _    -> "= ANY (" <> pgFmtArrayLiteralForField vals fld <> ") "

   Fts op lang val -> " " <> ftsOperator op <> "(" <> ftsLang lang <> unknownLiteral val <> ") "
 where
   ftsLang = maybe mempty (\l -> unknownLiteral l <> ", ")
   notOp = if hasNot then "NOT" else mempty
   star c = if c == '*' then '%' else c
   fmtQuant q val = case q of
    Just QuantAny -> "ANY(" <> val <> ")"
    Just QuantAll -> "ALL(" <> val <> ")"
    Nothing       -> val

pgFmtJoinCondition :: JoinCondition -> SQL.Snippet
pgFmtJoinCondition (JoinCondition (qi1, col1) (qi2, col2)) =
  pgFmtColumn qi1 col1 <> " = " <> pgFmtColumn qi2 col2

pgFmtLogicTree :: QualifiedIdentifier -> CoercibleLogicTree -> SQL.Snippet
pgFmtLogicTree qi (CoercibleExpr hasNot op forest) = SQL.sql notOp <> " (" <> intercalateSnippet (opSql op) (pgFmtLogicTree qi <$> forest) <> ")"
  where
    notOp =  if hasNot then "NOT" else mempty

    opSql And = " AND "
    opSql Or  = " OR "
pgFmtLogicTree qi (CoercibleStmnt flt) = pgFmtFilter qi flt

pgFmtJsonPath :: JsonPath -> SQL.Snippet
pgFmtJsonPath = \case
  []             -> mempty
  (JArrow x:xs)  -> "->" <> pgFmtJsonOperand x <> pgFmtJsonPath xs
  (J2Arrow x:xs) -> "->>" <> pgFmtJsonOperand x <> pgFmtJsonPath xs
  where
    pgFmtJsonOperand (JKey k) = unknownLiteral k
    pgFmtJsonOperand (JIdx i) = unknownLiteral i <> "::int"

pgFmtAs :: FieldName -> JsonPath -> Maybe Alias -> SQL.Snippet
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

groupF :: QualifiedIdentifier -> [SelectTerm] -> [SQL.Snippet] -> SQL.Snippet
groupF qi fields joinSelects =
  if all (\SelectTerm{selAggFunction=agg} -> isNothing agg) fields || all (\SelectTerm{selAggFunction=agg} -> isJust agg) fields && null joinSelects
    then
      mempty
    else
      " GROUP BY " <> intercalateSnippet ", " ((pgFmtGroup qi <$> (filter (\SelectTerm{selAggFunction=agg} -> isNothing agg) fields)) ++ joinSelects)

pgFmtGroup :: QualifiedIdentifier -> SelectTerm -> SQL.Snippet
pgFmtGroup _ SelectTerm{selAggFunction=Just _} = mempty
pgFmtGroup qi SelectTerm{selField=fld, selAggFunction=Nothing} = pgFmtField qi fld

countF :: SQL.Snippet -> Bool -> (SQL.Snippet, SQL.Snippet)
countF countQuery shouldCount =
  if shouldCount
    then (
        ", pgrst_source_count AS (" <> countQuery <> ")"
      , "(SELECT pg_catalog.count(*) FROM pgrst_source_count)" )
    else (
        mempty
      , "null::bigint")

returningF :: QualifiedIdentifier -> [FieldName] -> SQL.Snippet
returningF qi returnings =
  if null returnings
    then "RETURNING 1" -- For mutation cases where there's no ?select, we return 1 to know how many rows were modified
    else "RETURNING " <> intercalateSnippet ", " (pgFmtColumn qi <$> returnings)

limitOffsetF :: NonnegRange -> SQL.Snippet
limitOffsetF range =
  if range == allRange then mempty else "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit = maybe "ALL" (\l -> unknownEncoder (BS.pack $ show l)) $ rangeLimit range
    offset = unknownEncoder (BS.pack . show $ rangeOffset range)

responseHeadersF :: SQL.Snippet
responseHeadersF = currentSettingF "response.headers"

responseStatusF :: SQL.Snippet
responseStatusF = currentSettingF "response.status"

currentSettingF :: SQL.Snippet -> SQL.Snippet
currentSettingF setting =
  -- nullif is used because of https://gist.github.com/steve-chavez/8d7033ea5655096903f3b52f8ed09a15
  "nullif(current_setting('" <> setting <> "', true), '')"

mutRangeF :: QualifiedIdentifier -> [FieldName] -> (SQL.Snippet, SQL.Snippet)
mutRangeF mainQi rangeId =
  (
    intercalateSnippet " AND " $ (\col -> pgFmtColumn mainQi col <> " = " <> pgFmtColumn (QualifiedIdentifier mempty "pgrst_affected_rows") col) <$> rangeId
  , intercalateSnippet ", " (pgFmtColumn mainQi <$> rangeId)
  )

orderF :: QualifiedIdentifier -> [CoercibleOrderTerm] -> SQL.Snippet
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

    fmtPlanFmt PlanText = "FORMAT TEXT"
    fmtPlanFmt PlanJSON = "FORMAT JSON"

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

-- Investigate this
aggF :: Maybe Routine -> ResultAggregate -> SQL.Snippet
aggF rout = \case
  BuiltinAggJson             -> asJsonF rout False
  BuiltinAggArrayJsonStrip   -> asJsonF rout True
  BuiltinAggSingleJson strip -> asJsonSingleF rout strip
  BuiltinAggGeoJson          -> asGeoJsonF
  BuiltinAggCsv              -> asCsvF
  BuiltinAggXml    bField    -> asXmlF    bField
  BuiltinAggBinary bField    -> asBinaryF bField
  NoAgg                      -> "''::text"
