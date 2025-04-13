{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
{-|
Module      : PostgREST.Query.SqlFragment
Description : Helper functions for PostgREST.QueryBuilder.
-}
module PostgREST.Query.SqlFragment
  ( TrackedSnippet (..)
  , emptyTracked
  , trackParam
  , trackParams
  , rawSQL
  , fromSnippet
  , toSnippet
  , ToTrackedSnippet (..)
  , noLocationF
  , handlerF
  , countF
  , groupF
  , fromQi
  , limitOffsetF
  , locationF
  , orderF
  , pgFmtColumn
  , pgFmtFilter
  , pgFmtIdent
  , pgFmtJoinCondition
  , pgFmtLogicTree
  , pgFmtOrderTerm
  , pgFmtSelectItem
  , pgFmtSpreadSelectItem
  , pgFmtSpreadJoinSelectItem
  , fromJsonBodyF
  , responseHeadersF
  , responseStatusF
  , addConfigPgrstInserted
  , currentSettingF
  , returningF
  , singleParameter
  , sourceCTE
  , sourceCTEName
  , unknownEncoder
  , intercalateSnippet
  , explainF
  , setConfigWithConstantName
  , setConfigWithDynamicName
  , setConfigWithConstantNameJSON
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

import Data.Foldable     (foldr1)
import NeatInterpolation (trimming)

import PostgREST.ApiRequest.Types        (AggregateFunction (..),
                                          Alias, Cast,
                                          FtsOperator (..),
                                          IsVal (..),
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
                                          SimpleOperator (..))
import PostgREST.MediaType               (MTVndPlanFormat (..),
                                          MTVndPlanOption (..))
import PostgREST.Plan.ReadPlan           (JoinCondition (..))
import PostgREST.Plan.Types              (CoercibleField (..),
                                          CoercibleFilter (..),
                                          CoercibleLogicTree (..),
                                          CoercibleOrderTerm (..),
                                          CoercibleSelectField (..),
                                          RelSelectField (..),
                                          SpreadSelectField (..),
                                          ToTsVector (..),
                                          unknownField)
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          rangeLimit, rangeOffset)
import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          RelIdentifier (..))
import PostgREST.SchemaCache.Routine     (MediaHandler (..),
                                          Routine (..),
                                          funcReturnsScalar,
                                          funcReturnsSetOfScalar,
                                          funcReturnsSingleComposite)

import Protolude hiding (Sum, cast)

-- | Wrapper on top of Hasql.Snippet which also able to track parameters encoded to the query
data TrackedSnippet = TrackedSnippet
  { snippet :: SQL.Snippet
  , params  :: [Maybe ByteString]
  }

-- | Create an empty tracked snippet
emptyTracked :: TrackedSnippet
emptyTracked = TrackedSnippet mempty []

-- | Create a tracked snippet from raw SQL with no parameters
rawSQL :: ByteString -> TrackedSnippet
rawSQL sql = TrackedSnippet (SQL.sql sql) []

-- | Track a single parameter with its SQL snippet
trackParam :: ByteString -> SQL.Snippet -> TrackedSnippet
trackParam param snip = TrackedSnippet snip [Just param]

-- | Track multiple parameters with their descriptions and encoder
trackParams :: [Maybe ByteString] -> SQL.Snippet -> TrackedSnippet
trackParams params snip = TrackedSnippet snip params

-- | Convert SQL.Snippet to TrackedSnippet (for backward compatibility)
fromSnippet :: SQL.Snippet -> TrackedSnippet
fromSnippet snip = TrackedSnippet snip []

-- | Convert TrackedSnippet to SQL.Snippet (for backward compatibility)
toSnippet :: TrackedSnippet -> SQL.Snippet
toSnippet = snippet

-- | Helper to allow SQL.Snippet and TrackedSnippet to coexist during transition
class ToTrackedSnippet a where
  toTrackedSnippet :: a -> TrackedSnippet

instance ToTrackedSnippet SQL.Snippet where
  toTrackedSnippet = fromSnippet

instance ToTrackedSnippet TrackedSnippet where
  toTrackedSnippet t = t

-- | Concatenation for TrackedSnippet
instance Semigroup TrackedSnippet where
  TrackedSnippet s1 p1 <> TrackedSnippet s2 p2 = TrackedSnippet (s1 <> s2) (p1 <> p2)

-- | Empty element for TrackedSnippet
instance Monoid TrackedSnippet where
  mempty = emptyTracked

sourceCTEName :: Text
sourceCTEName = "pgrst_source"

sourceCTE :: TrackedSnippet
sourceCTE = rawSQL "pgrst_source"

noLocationF :: TrackedSnippet
noLocationF = rawSQL "array[]::text[]"

simpleOperator :: SimpleOperator -> TrackedSnippet
simpleOperator = \case
  OpNotEqual        -> rawSQL "<>"
  OpContains        -> rawSQL "@>"
  OpContained       -> rawSQL "<@"
  OpOverlap         -> rawSQL "&&"
  OpStrictlyLeft    -> rawSQL "<<"
  OpStrictlyRight   -> rawSQL ">>"
  OpNotExtendsRight -> rawSQL "&<"
  OpNotExtendsLeft  -> rawSQL "&>"
  OpAdjacent        -> rawSQL "-|-"

quantOperator :: QuantOperator -> TrackedSnippet
quantOperator = \case
  OpEqual            -> rawSQL "="
  OpGreaterThanEqual -> rawSQL ">="
  OpGreaterThan      -> rawSQL ">"
  OpLessThanEqual    -> rawSQL "<="
  OpLessThan         -> rawSQL "<"
  OpLike             -> rawSQL "like"
  OpILike            -> rawSQL "ilike"
  OpMatch            -> rawSQL "~"
  OpIMatch           -> rawSQL "~*"

ftsOperator :: FtsOperator -> TrackedSnippet
ftsOperator = \case
  FilterFts          -> rawSQL "@@ to_tsquery"
  FilterFtsPlain     -> rawSQL "@@ plainto_tsquery"
  FilterFtsPhrase    -> rawSQL "@@ phraseto_tsquery"
  FilterFtsWebsearch -> rawSQL "@@ websearch_to_tsquery"

singleParameter :: Maybe LBS.ByteString -> ByteString -> TrackedSnippet
singleParameter body typ =
  let strictBody = LBS.toStrict <$> body
      snippet =
        if typ == "bytea"
          -- TODO: Hasql fails when using HE.unknown with bytea(pg tries to utf8 encode).
          then SQL.encoderAndParam (HE.nullable HE.bytea) strictBody
          else SQL.encoderAndParam (HE.nullable HE.unknown) strictBody <> "::" <> SQL.sql typ
   in TrackedSnippet snippet [strictBody]

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
pgFmtIdent :: Text -> TrackedSnippet
pgFmtIdent x = rawSQL . encodeUtf8 $ escapeIdent x

escapeIdent :: Text -> Text
escapeIdent x = "\"" <> T.replace "\"" "\"\"" (trimNullChars x) <> "\""

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
escapeIdentList schemas = BS.intercalate ", " $ encodeUtf8 . escapeIdent <$> schemas

asCsvF :: TrackedSnippet
asCsvF = asCsvHeaderF <> rawSQL " || '\n' || " <> asCsvBodyF
  where
    asCsvHeaderF =
      rawSQL "(SELECT coalesce(string_agg(a.k, ','), '')" <>
      rawSQL "  FROM (" <>
      rawSQL "    SELECT json_object_keys(r)::text as k" <>
      rawSQL "    FROM ( " <>
      rawSQL "      SELECT row_to_json(hh) as r from " <> sourceCTE <> rawSQL " as hh limit 1" <>
      rawSQL "    ) s" <>
      rawSQL "  ) a" <>
      rawSQL ")"
    asCsvBodyF = rawSQL "coalesce(string_agg(substring(_postgrest_t::text, 2, length(_postgrest_t::text) - 2), '\n'), '')"

addNullsToSnip :: Bool -> TrackedSnippet -> TrackedSnippet
addNullsToSnip strip (TrackedSnippet snip params) =
  if strip
    then
      TrackedSnippet (SQL.sql "json_strip_nulls(" <> snip <> SQL.sql ")") params
    else
      TrackedSnippet snip params

asJsonSingleF :: Maybe Routine -> Bool -> TrackedSnippet
asJsonSingleF rout strip
  | returnsScalar = rawSQL "coalesce(" <> addNullsToSnip strip (rawSQL "json_agg(_postgrest_t.pgrst_scalar)->0") <> rawSQL ", 'null')"
  | otherwise     = rawSQL "coalesce(" <> addNullsToSnip strip (rawSQL "json_agg(_postgrest_t)->0") <> rawSQL ", 'null')"
  where
    returnsScalar = maybe False funcReturnsScalar rout

asJsonF :: Maybe Routine -> Bool -> TrackedSnippet
asJsonF rout strip
  | returnsSingleComposite    = rawSQL "coalesce(" <> addNullsToSnip strip (rawSQL "json_agg(_postgrest_t)->0") <> rawSQL ", 'null')"
  | returnsScalar             = rawSQL "coalesce(" <> addNullsToSnip strip (rawSQL "json_agg(_postgrest_t.pgrst_scalar)->0") <> rawSQL ", 'null')"
  | returnsSetOfScalar        = rawSQL "coalesce(" <> addNullsToSnip strip (rawSQL "json_agg(_postgrest_t.pgrst_scalar)") <> rawSQL ", '[]')"
  | otherwise                 = rawSQL "coalesce(" <> addNullsToSnip strip (rawSQL "json_agg(_postgrest_t)") <> rawSQL ", '[]')"
  where
    (returnsSingleComposite, returnsScalar, returnsSetOfScalar) = case rout of
      Just r  -> (funcReturnsSingleComposite r, funcReturnsScalar r, funcReturnsSetOfScalar r)
      Nothing -> (False, False, False)

asGeoJsonF :: TrackedSnippet
asGeoJsonF = rawSQL "json_build_object('type', 'FeatureCollection', 'features', coalesce(json_agg(ST_AsGeoJSON(_postgrest_t)::json), '[]'))"

customFuncF :: Maybe Routine -> QualifiedIdentifier -> RelIdentifier -> TrackedSnippet
customFuncF rout funcQi _
  | (funcReturnsScalar <$> rout) == Just True = fromQi funcQi <> rawSQL "(_postgrest_t.pgrst_scalar)"
customFuncF _ funcQi RelAnyElement            = fromQi funcQi <> rawSQL "(_postgrest_t)"
customFuncF _ funcQi (RelId target)           = fromQi funcQi <> rawSQL "(_postgrest_t::" <> fromQi target <> rawSQL ")"

locationF :: [Text] -> TrackedSnippet
locationF pKeys = rawSQL $ encodeUtf8 [trimming|(
  WITH data AS (SELECT row_to_json(_) AS row FROM ${sourceCTEName} AS _ LIMIT 1)
  SELECT array_agg(json_data.key || '=' || coalesce('eq.' || json_data.value, 'is.null'))
  FROM data CROSS JOIN json_each_text(data.row) AS json_data
  WHERE json_data.key IN ('${fmtPKeys}')
)|]
  where
    fmtPKeys = T.intercalate "','" pKeys

fromQi :: QualifiedIdentifier -> TrackedSnippet
fromQi t = (if T.null s then pgFmtIdent n else pgFmtIdent s <> rawSQL ".") <> pgFmtIdent n
  where
    n = qiName t
    s = qiSchema t

pgFmtColumn :: QualifiedIdentifier -> Text -> TrackedSnippet
pgFmtColumn table "*" = fromQi table <> rawSQL ".*"
pgFmtColumn table c   = fromQi table <> rawSQL "." <> pgFmtIdent c

pgFmtCallUnary :: Text -> TrackedSnippet -> TrackedSnippet
pgFmtCallUnary f (TrackedSnippet x params) = TrackedSnippet (SQL.sql (encodeUtf8 f) <> "(" <> x <> ")") params

pgFmtField :: QualifiedIdentifier -> CoercibleField -> TrackedSnippet
pgFmtField table cf = case cfToTsVector cf of
  Just (ToTsVector lang) -> rawSQL "to_tsvector(" <> pgFmtFtsLang lang <> fmtFld <> rawSQL ")"
  _                      -> fmtFld
  where
    fmtFld = case cf of
      CoercibleField{cfFullRow=True}                                          -> pgFmtIdent (qiName table)
      CoercibleField{cfName=fn, cfJsonPath=[]}                                -> pgFmtColumn table fn
      CoercibleField{cfName=fn, cfToJson=doToJson, cfJsonPath=jp} | doToJson  -> rawSQL "to_jsonb(" <> pgFmtColumn table fn <> rawSQL ")" <> pgFmtJsonPath jp
                                                                  | otherwise -> pgFmtColumn table fn <> pgFmtJsonPath jp

-- Select the value of a named element from a table, applying its optional coercion mapping if any.
pgFmtTableCoerce :: QualifiedIdentifier -> CoercibleField -> TrackedSnippet
pgFmtTableCoerce table fld@(CoercibleField{cfTransform=(Just formatterProc)}) = pgFmtCallUnary formatterProc (pgFmtField table fld)
pgFmtTableCoerce table f = pgFmtField table f

-- | Like the previous but now we just have a name so no namespace or JSON paths.
pgFmtCoerceNamed :: CoercibleField -> TrackedSnippet
pgFmtCoerceNamed CoercibleField{cfName=fn, cfTransform=(Just formatterProc)} = pgFmtCallUnary formatterProc (pgFmtIdent fn) <> rawSQL " AS " <> pgFmtIdent fn
pgFmtCoerceNamed CoercibleField{cfName=fn} = pgFmtIdent fn

pgFmtSelectItem :: QualifiedIdentifier -> CoercibleSelectField -> TrackedSnippet
pgFmtSelectItem table CoercibleSelectField{csField=fld, csAggFunction=agg, csAggCast=aggCast, csCast=cast, csAlias=alias} =
  pgFmtApplyAggregate agg aggCast (pgFmtApplyCast cast (pgFmtTableCoerce table fld)) <> pgFmtAs alias

pgFmtSpreadSelectItem :: Alias -> SpreadSelectField -> TrackedSnippet
pgFmtSpreadSelectItem aggAlias SpreadSelectField{ssSelName, ssSelAggFunction, ssSelAggCast, ssSelAlias} =
  pgFmtApplyAggregate ssSelAggFunction ssSelAggCast (pgFmtFullSelName aggAlias ssSelName) <> pgFmtAs ssSelAlias

pgFmtApplyAggregate :: Maybe AggregateFunction -> Maybe Cast -> TrackedSnippet -> TrackedSnippet
pgFmtApplyAggregate Nothing _ snippet = snippet
pgFmtApplyAggregate (Just agg) aggCast (TrackedSnippet snippet params) =
  pgFmtApplyCast aggCast (TrackedSnippet aggregatedSnippet params)
  where
    convertAggFunction :: AggregateFunction -> SQL.Snippet
    -- Convert from e.g. Sum (the data type) to SUM
    convertAggFunction = SQL.sql . BS.map toUpper . BS.pack . show
    aggregatedSnippet = convertAggFunction agg <> "(" <> snippet <> ")"

pgFmtSpreadJoinSelectItem :: Alias -> [CoercibleOrderTerm] -> SpreadSelectField -> TrackedSnippet
pgFmtSpreadJoinSelectItem aggAlias order SpreadSelectField{ssSelName, ssSelAlias} =
  rawSQL "COALESCE(json_agg(" <> fmtField <> rawSQL " " <> fmtOrder <> rawSQL "),'[]')::jsonb" <> rawSQL " AS " <> fmtAlias
  where
    fmtField = pgFmtFullSelName aggAlias ssSelName
    fmtOrder = orderF (QualifiedIdentifier "" aggAlias) order
    fmtAlias = pgFmtIdent (fromMaybe ssSelName ssSelAlias)

pgFmtApplyCast :: Maybe Cast -> TrackedSnippet -> TrackedSnippet
pgFmtApplyCast Nothing snippet = snippet
-- Ideally we'd quote the cast with "pgFmtIdent cast". However, that would invalidate common casts such as "int", "bigint", etc.
-- Try doing: `select 1::"bigint"` - it'll err, using "int8" will work though. There's some parser magic that pg does that's invalidated when quoting.
-- Not quoting should be fine, we validate the input on Parsers.
pgFmtApplyCast (Just cast) (TrackedSnippet snippet params) = TrackedSnippet (SQL.sql "CAST( " <> snippet <> SQL.sql " AS " <> SQL.sql (encodeUtf8 cast) <> SQL.sql " )") params

pgFmtFullSelName :: Alias -> FieldName -> TrackedSnippet
pgFmtFullSelName aggAlias fieldName = case fieldName of
  "*" -> pgFmtIdent aggAlias <> rawSQL ".*"
  _   -> pgFmtIdent aggAlias <> rawSQL "." <> pgFmtIdent fieldName

-- TODO: At this stage there shouldn't be a Maybe since ApiRequest should ensure that an INSERT/UPDATE has a body
fromJsonBodyF :: Maybe LBS.ByteString -> [CoercibleField] -> Bool -> Bool -> Bool -> TrackedSnippet
fromJsonBodyF body fields includeSelect includeLimitOne includeDefaults =
  (if includeSelect then rawSQL "SELECT " <> namedCols <> rawSQL " " else emptyTracked) <>
  rawSQL "FROM (SELECT " <> jsonPlaceHolder <> rawSQL " AS json_data) pgrst_payload, " <> 
  (if includeDefaults
    then if isJsonObject
      then rawSQL "LATERAL (SELECT " <> defsJsonb <> rawSQL " || pgrst_payload.json_data AS val) pgrst_json_defs, "
      else rawSQL "LATERAL (SELECT jsonb_agg(" <> defsJsonb <> rawSQL " || elem) AS val from jsonb_array_elements(pgrst_payload.json_data) elem) pgrst_json_defs, "
    else emptyTracked ) <> 
  rawSQL "LATERAL (SELECT " <> parsedCols <> rawSQL " FROM " <> 
    (if null fields -- when json keys are empty, e.g. when payload is `{}` or `[{}, {}]`
      then
        if isJsonObject
          then rawSQL "(values(1)) _ "                                                                                 -- only 1 row for an empty json object '{}'
          else rawSQL (encodeUtf8 jsonArrayElementsF) <> rawSQL "(" <> rawSQL (encodeUtf8 finalBodyF) <> rawSQL ") _ " -- extract rows of a json array of empty objects `[{}, {}]`
      else rawSQL (encodeUtf8 jsonToRecordsetF) <> rawSQL "(" <> rawSQL (encodeUtf8 finalBodyF) <> rawSQL ") AS _(" <> typedCols <> rawSQL ") " <> if includeLimitOne then rawSQL "LIMIT 1" else emptyTracked
    ) <>
    rawSQL ") pgrst_body "
  where
    namedCols = intercalateSnippet ", " $ fromQi . QualifiedIdentifier "pgrst_body" . cfName <$> fields
    parsedCols = intercalateSnippet ", " $ pgFmtCoerceNamed <$> fields
    typedCols = intercalateSnippet ", " $ pgFmtIdent . cfName <> const (rawSQL " ") <> rawSQL . encodeUtf8 . cfIRType <$> fields
    defsJsonb = rawSQL $ "jsonb_build_object(" <> BS.intercalate "," fieldsWDefaults <> ")"
    fieldsWDefaults = mapMaybe ( \case
        CoercibleField{cfName = nam, cfDefault = Just def} -> Just $ encodeUtf8 (pgFmtLit nam <> ", " <> def)
        CoercibleField{cfDefault = Nothing} -> Nothing
      ) fields
    (finalBodyF, jsonArrayElementsF, jsonToRecordsetF) =
      if includeDefaults
        then ("pgrst_json_defs.val", "jsonb_array_elements", if isJsonObject then "jsonb_to_record" else "jsonb_to_recordset")
        else ("pgrst_payload.json_data", "json_array_elements", if isJsonObject then "json_to_record" else "json_to_recordset")
    jsonPlaceHolder = TrackedSnippet (SQL.encoderAndParam (HE.nullable $ if includeDefaults then HE.jsonbLazyBytes else HE.jsonLazyBytes) body) [LBS.toStrict <$> body]
    isJsonObject = -- light validation as pg's json_to_record(set) already validates that the body is valid JSON. We just need to know whether the body looks like an object or not.
      let
        insignificantWhitespace = [32,9,10,13] --" \t\n\r" [32,9,10,13] https://datatracker.ietf.org/doc/html/rfc8259#section-2
      in
      LBS.take 1 (LBS.dropWhile (`elem` insignificantWhitespace) (fromMaybe mempty body)) == "{"

pgFmtOrderTerm :: QualifiedIdentifier -> CoercibleOrderTerm -> TrackedSnippet
pgFmtOrderTerm qi ot =
  fmtOTerm ot <> rawSQL " " <> 
  rawSQL ( BS.unwords [
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
pgFmtUnknownLiteralForField :: TrackedSnippet -> CoercibleField -> TrackedSnippet
pgFmtUnknownLiteralForField value CoercibleField{cfTransform=(Just parserProc)} = pgFmtCallUnary parserProc value
-- But when no transform is requested, we just use the literal as-is.
pgFmtUnknownLiteralForField value _ = value

-- | Array version of the above, used by ANY().
pgFmtArrayLiteralForField :: [Text] -> CoercibleField -> TrackedSnippet
-- When a transformation is requested, we need to apply the transformation to each element of the array. This could be done by just making a query with `parser(value)` for each value, but may lead to huge query lengths. Imagine `data_representations.color_from_text('...'::text)` for repeated for a hundred values. Instead we use `unnest()` to unpack a standard array literal and then apply the transformation to each element, like a map.
-- Note the literals will be treated as text since in every case when we use ANY() the parameters are textual (coming from a query string). We want to rely on the `text->domain` parser to do the right thing.
pgFmtArrayLiteralForField values CoercibleField{cfTransform = (Just parserProc)} = rawSQL "(SELECT " <> pgFmtCallUnary parserProc (rawSQL "unnest(" <> unknownLiteral (pgBuildArrayLiteral values) <> rawSQL "::text[])") <> rawSQL ")"
-- When no transformation is requested, we don't need a subquery.
pgFmtArrayLiteralForField values _ = unknownLiteral (pgBuildArrayLiteral values)


pgFmtFilter :: QualifiedIdentifier -> CoercibleFilter -> TrackedSnippet
pgFmtFilter _ (CoercibleFilterNullEmbed hasNot fld) = pgFmtIdent fld <> rawSQL " IS " <> rawSQL (if not hasNot then "NOT " else mempty) <> rawSQL "DISTINCT FROM NULL"
pgFmtFilter _ (CoercibleFilter _ (NoOpExpr _)) = emptyTracked -- TODO unreachable because NoOpExpr is filtered on QueryParams
pgFmtFilter table (CoercibleFilter fld (OpExpr hasNot oper)) = rawSQL notOp <> rawSQL " " <> pgFmtField table fld <> case oper of
   Op op val  -> rawSQL " " <> simpleOperator op <> rawSQL " " <> pgFmtUnknownLiteralForField (unknownLiteral val) fld

   OpQuant op quant val -> rawSQL " " <> quantOperator op <> rawSQL " " <> case op of
     OpLike  -> fmtQuant quant $ unknownLiteral (T.map star val)
     OpILike -> fmtQuant quant $ unknownLiteral (T.map star val)
     _       -> fmtQuant quant $ pgFmtUnknownLiteralForField (unknownLiteral val) fld

   -- IS cannot be prepared. `PREPARE boolplan AS SELECT * FROM projects where id IS $1` will give a syntax error.
   -- The above can be fixed by using `PREPARE boolplan AS SELECT * FROM projects where id IS NOT DISTINCT FROM $1;`
   -- However that would not accept the TRUE/FALSE/NULL/"NOT NULL"/UNKNOWN keywords. See: https://stackoverflow.com/questions/6133525/proper-way-to-set-preparedstatement-parameter-to-null-under-postgres.
   -- This is why `IS` operands are whitelisted at the Parsers.hs level
   Is isVal -> rawSQL " IS " <> 
      case isVal of
        IsNull       -> rawSQL "NULL"
        IsNotNull    -> rawSQL "NOT NULL"
        IsTriTrue    -> rawSQL "TRUE"
        IsTriFalse   -> rawSQL "FALSE"
        IsTriUnknown -> rawSQL "UNKNOWN"

   IsDistinctFrom val -> rawSQL " IS DISTINCT FROM " <> unknownLiteral val

   -- We don't use "IN", we use "= ANY". IN has the following disadvantages:
   -- + No way to use an empty value on IN: "col IN ()" is invalid syntax. With ANY we can do "= ANY('{}')"
   -- + Can invalidate prepared statements: multiple parameters on an IN($1, $2, $3) will lead to using different prepared statements and not take advantage of caching.
   In vals -> rawSQL " " <> case vals of
      [""] -> rawSQL "= ANY('{}') "
      _    -> rawSQL "= ANY (" <> pgFmtArrayLiteralForField vals fld <> rawSQL ") "

   Fts op lang val -> rawSQL " " <> ftsOperator op <> rawSQL "(" <> pgFmtFtsLang lang <> unknownLiteral val <> rawSQL ") "
 where
   notOp = if hasNot then "NOT" else mempty
   star c = if c == '*' then '%' else c
   fmtQuant q val = case q of
    Just QuantAny -> rawSQL "ANY(" <> val <> rawSQL ")"
    Just QuantAll -> rawSQL "ALL(" <> val <> rawSQL ")"
    Nothing       -> val

pgFmtFtsLang :: Maybe Text -> TrackedSnippet
pgFmtFtsLang = maybe emptyTracked (\l -> unknownLiteral l <> rawSQL ", ")

pgFmtJoinCondition :: JoinCondition -> TrackedSnippet
pgFmtJoinCondition (JoinCondition (qi1, col1) (qi2, col2)) =
  pgFmtColumn qi1 col1 <> rawSQL " = " <> pgFmtColumn qi2 col2

pgFmtLogicTree :: QualifiedIdentifier -> CoercibleLogicTree -> TrackedSnippet
pgFmtLogicTree qi (CoercibleExpr hasNot op forest) = rawSQL notOp <> rawSQL " (" <> intercalateSnippet (opSql op) (pgFmtLogicTree qi <$> forest) <> rawSQL ")"
  where
    notOp =  if hasNot then "NOT" else mempty

    opSql And = " AND "
    opSql Or  = " OR "
pgFmtLogicTree qi (CoercibleStmnt flt) = pgFmtFilter qi flt

pgFmtJsonPath :: JsonPath -> TrackedSnippet
pgFmtJsonPath = \case
  []             -> emptyTracked
  (JArrow x:xs)  -> rawSQL "->" <> pgFmtJsonOperand x <> pgFmtJsonPath xs
  (J2Arrow x:xs) -> rawSQL "->>" <> pgFmtJsonOperand x <> pgFmtJsonPath xs
 where
    pgFmtJsonOperand (JKey k) = unknownLiteral k
    pgFmtJsonOperand (JIdx i) = unknownLiteral i <> rawSQL "::int"

pgFmtAs :: Maybe Alias -> TrackedSnippet
pgFmtAs Nothing      = emptyTracked
pgFmtAs (Just alias) = rawSQL " AS " <> pgFmtIdent alias

groupF :: QualifiedIdentifier -> [CoercibleSelectField] -> [RelSelectField] -> TrackedSnippet
groupF qi select relSelect
  | (noSelectsAreAggregated && noRelSelectsAreAggregated) || null groupTerms = emptyTracked
  | otherwise = rawSQL " GROUP BY " <> intercalateSnippet ", " groupTerms
 where
    noSelectsAreAggregated = null $ [s | s@(CoercibleSelectField { csAggFunction = Just _ }) <- select]
    noRelSelectsAreAggregated = all (\case Spread sels _ -> all (isNothing . ssSelAggFunction) sels; _ -> True) relSelect
    groupTermsFromSelect = mapMaybe (pgFmtGroup qi) select
    groupTermsFromRelSelect = mapMaybe groupTermFromRelSelectField relSelect
    groupTerms = groupTermsFromSelect ++ groupTermsFromRelSelect

groupTermFromRelSelectField :: RelSelectField -> Maybe TrackedSnippet
groupTermFromRelSelectField (JsonEmbed { rsSelName }) =
  Just $ pgFmtIdent rsSelName
groupTermFromRelSelectField (Spread{rsSpreadSel, rsAggAlias}) =
  if null groupTerms
  then Nothing
  else
    Just $ intercalateSnippet ", " groupTerms
 where
    processField :: SpreadSelectField -> Maybe TrackedSnippet
    processField SpreadSelectField{ssSelAggFunction = Just _} = Nothing
    processField SpreadSelectField{ssSelName, ssSelAlias} =
      Just $ pgFmtIdent rsAggAlias <> rawSQL "." <> pgFmtIdent (fromMaybe ssSelName ssSelAlias)
    groupTerms = mapMaybe processField rsSpreadSel

pgFmtGroup :: QualifiedIdentifier -> CoercibleSelectField -> Maybe TrackedSnippet
pgFmtGroup _  CoercibleSelectField{csAggFunction=Just _} = Nothing
pgFmtGroup _  CoercibleSelectField{csAlias=Just alias, csAggFunction=Nothing} = Just $ pgFmtIdent alias
pgFmtGroup qi CoercibleSelectField{csField=fld, csAlias=Nothing, csAggFunction=Nothing} = Just $ pgFmtField qi fld

countF :: TrackedSnippet -> Bool -> (TrackedSnippet, TrackedSnippet)
countF countQuery shouldCount =
  if shouldCount
    then (
        rawSQL ", pgrst_source_count AS (" <> countQuery <> rawSQL ")"
      , rawSQL "(SELECT pg_catalog.count(*) FROM pgrst_source_count)" )
    else (
        emptyTracked
      , rawSQL "null::bigint")

returningF :: QualifiedIdentifier -> [FieldName] -> TrackedSnippet
returningF qi returnings =
  if null returnings
    then rawSQL "RETURNING 1" -- For mutation cases where there's no ?select, we return 1 to know how many rows were modified
    else rawSQL "RETURNING " <> intercalateSnippet ", " (pgFmtColumn qi <$> returnings)

limitOffsetF :: NonnegRange -> TrackedSnippet
limitOffsetF range =
  if range == allRange then emptyTracked else rawSQL "LIMIT " <> limit <> rawSQL " OFFSET " <> offset
  where
    limit = maybe (rawSQL "ALL") (\l -> unknownEncoder (BS.pack $ show l)) $ rangeLimit range
    offset = unknownEncoder (BS.pack . show $ rangeOffset range)

responseHeadersF :: TrackedSnippet
responseHeadersF = currentSettingF (rawSQL "response.headers")

responseStatusF :: TrackedSnippet
responseStatusF = currentSettingF (rawSQL "response.status")

addConfigPgrstInserted :: Bool -> TrackedSnippet
addConfigPgrstInserted add =
  let (symbol, num) =  if add then (rawSQL "+", rawSQL "0") else (rawSQL "-", rawSQL "-1") in
  rawSQL "set_config('pgrst.inserted', (coalesce(" <> currentSettingF (rawSQL "pgrst.inserted") <> rawSQL "::int, 0) " <> symbol <> rawSQL " 1)::text, true) <> '" <> num <> rawSQL "'"

currentSettingF :: TrackedSnippet -> TrackedSnippet
currentSettingF setting =
  -- nullif is used because of https://gist.github.com/steve-chavez/8d7033ea5655096903f3b52f8ed09a15
  rawSQL "nullif(current_setting('" <> setting <> rawSQL "', true), '')"

orderF :: QualifiedIdentifier -> [CoercibleOrderTerm] -> TrackedSnippet
orderF _ []   = emptyTracked
orderF qi ordts = rawSQL "ORDER BY " <> intercalateSnippet ", " (pgFmtOrderTerm qi <$> ordts)

-- Hasql Snippet utilities
unknownEncoder :: ByteString -> TrackedSnippet
unknownEncoder param = TrackedSnippet (SQL.encoderAndParam (HE.nonNullable HE.unknown) param) [Just param]

unknownLiteral :: Text -> TrackedSnippet
unknownLiteral = unknownEncoder . encodeUtf8

intercalateSnippet :: ByteString -> [TrackedSnippet] -> TrackedSnippet
intercalateSnippet _ [] = emptyTracked
intercalateSnippet frag snippets = foldr1 (\a b -> a <> rawSQL frag <> b) snippets

explainF :: MTVndPlanFormat -> [MTVndPlanOption] -> TrackedSnippet -> TrackedSnippet
explainF fmt opts (TrackedSnippet snip params) = TrackedSnippet
  ( "EXPLAIN (" <>
    SQL.sql (BS.intercalate ", " (fmtPlanFmt fmt : (fmtPlanOpt <$> opts))) <>
  ") " <> snip ) params
 where
    fmtPlanOpt :: MTVndPlanOption -> BS.ByteString
    fmtPlanOpt PlanAnalyze  = "ANALYZE"
    fmtPlanOpt PlanVerbose  = "VERBOSE"
    fmtPlanOpt PlanSettings = "SETTINGS"
    fmtPlanOpt PlanBuffers  = "BUFFERS"
    fmtPlanOpt PlanWAL      = "WAL"

    fmtPlanFmt PlanText = "FORMAT TEXT"
    fmtPlanFmt PlanJSON = "FORMAT JSON"

-- | Do a pg set_config(setting, value, true) call. This is equivalent to a SET LOCAL.
setConfigLocal :: (TrackedSnippet, ByteString) -> TrackedSnippet
setConfigLocal (k, v) =
  rawSQL "set_config(" <> k <> rawSQL ", " <> unknownEncoder v <> rawSQL ", true)"

-- | For when the settings are hardcoded and not parameterized
setConfigWithConstantName :: (SQL.Snippet, ByteString) -> TrackedSnippet
setConfigWithConstantName (k, v) = setConfigLocal (TrackedSnippet ("'" <> k <> "'") [], v)

-- | For when the settings need to be parameterized
setConfigWithDynamicName :: (ByteString, ByteString) -> TrackedSnippet
setConfigWithDynamicName (k, v) =
  setConfigLocal (unknownEncoder k, v)

-- | Starting from PostgreSQL v14, some characters are not allowed for config names (mostly affecting headers with "-").
-- | A JSON format string is used to avoid this problem. See https://github.com/PostgREST/postgrest/issues/1857
setConfigWithConstantNameJSON :: SQL.Snippet -> [(ByteString, ByteString)] -> [TrackedSnippet]
setConfigWithConstantNameJSON prefix keyVals = [setConfigWithConstantName (prefix, gucJsonVal keyVals)]
  where
    gucJsonVal :: [(ByteString, ByteString)] -> ByteString
    gucJsonVal = LBS.toStrict . JSON.encode . HM.fromList . arrayByteStringToText
    arrayByteStringToText :: [(ByteString, ByteString)] -> [(Text,Text)]
    arrayByteStringToText keyVal = (T.decodeUtf8 *** T.decodeUtf8) <$> keyVal

handlerF :: Maybe Routine -> MediaHandler -> TrackedSnippet
handlerF rout = \case
  BuiltinAggArrayJsonStrip   -> asJsonF rout True
  BuiltinAggSingleJson strip -> asJsonSingleF rout strip
  BuiltinOvAggJson           -> asJsonF rout False
  BuiltinOvAggGeoJson        -> asGeoJsonF
  BuiltinOvAggCsv            -> asCsvF
  CustomFunc funcQi target   -> customFuncF rout funcQi target
  NoAgg                      -> rawSQL "''::text"
