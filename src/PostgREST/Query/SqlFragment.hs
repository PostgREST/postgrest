{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
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
  , asJsonF
  , asJsonSingleF
  , countF
  , fromQi
  , ftsOperators
  , limitOffsetF
  , locationF
  , normalizedBody
  , operators
  , pgFmtColumn
  , pgFmtIdent
  , pgFmtJoinCondition
  , pgFmtLogicTree
  , pgFmtOrderTerm
  , pgFmtSelectItem
  , responseHeadersF
  , responseStatusF
  , returningF
  , selectBody
  , singleParameter
  , sourceCTEName
  , unknownEncoder
  , intercalateSnippet
  ) where

import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as BL
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text                       as T
import qualified Hasql.DynamicStatements.Snippet as H
import qualified Hasql.Encoders                  as HE

import Data.Foldable                 (foldr1)
import Text.InterpolatedString.Perl6 (qc)

import PostgREST.Config.PgVersion        (PgVersion, pgVersion96)
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier (..))
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          rangeLimit, rangeOffset)
import PostgREST.Request.Types           (Alias, Field, Filter (..),
                                          JoinCondition (..),
                                          JsonOperand (..),
                                          JsonOperation (..),
                                          JsonPath, LogicTree (..),
                                          OpExpr (..), Operation (..),
                                          OrderTerm (..), SelectItem)

import Protolude      hiding (cast, toS)
import Protolude.Conv (toS)


-- | A part of a SQL query that cannot be executed independently
type SqlFragment = ByteString

noLocationF :: SqlFragment
noLocationF = "array[]::text[]"

sourceCTEName :: SqlFragment
sourceCTEName = "pgrst_source"

operators :: HM.HashMap Text SqlFragment
operators = HM.union (HM.fromList [
  ("eq", "="),
  ("gte", ">="),
  ("gt", ">"),
  ("lte", "<="),
  ("lt", "<"),
  ("neq", "<>"),
  ("like", "LIKE"),
  ("ilike", "ILIKE"),
  ("in", "IN"),
  ("is", "IS"),
  ("cs", "@>"),
  ("cd", "<@"),
  ("ov", "&&"),
  ("sl", "<<"),
  ("sr", ">>"),
  ("nxr", "&<"),
  ("nxl", "&>"),
  ("adj", "-|-")]) ftsOperators

ftsOperators :: HM.HashMap Text SqlFragment
ftsOperators = HM.fromList [
  ("fts", "@@ to_tsquery"),
  ("plfts", "@@ plainto_tsquery"),
  ("phfts", "@@ phraseto_tsquery"),
  ("wfts", "@@ websearch_to_tsquery")
  ]

-- |
-- These CTEs convert a json object into a json array, this way we can use json_populate_recordset for all json payloads
-- Otherwise we'd have to use json_populate_record for json objects and json_populate_recordset for json arrays
-- We do this in SQL to avoid processing the JSON in application code
-- TODO: At this stage there shouldn't be a Maybe since ApiRequest should ensure that an INSERT/UPDATE has a body
normalizedBody :: Maybe BL.ByteString -> H.Snippet
normalizedBody body =
  "pgrst_payload AS (SELECT " <> jsonPlaceHolder <> " AS json_data), " <>
  H.sql (BS.unwords [
    "pgrst_body AS (",
      "SELECT",
        "CASE WHEN json_typeof(json_data) = 'array'",
          "THEN json_data",
          "ELSE json_build_array(json_data)",
        "END AS val",
      "FROM pgrst_payload)"])
  where
    jsonPlaceHolder = H.encoderAndParam (HE.nullable HE.unknown) (toS <$> body) <> "::json"

singleParameter :: Maybe BL.ByteString -> ByteString -> H.Snippet
singleParameter body typ =
  if typ == "bytea"
    then H.encoderAndParam (HE.nullable HE.bytea) (toS <$> body) -- needed because bytea fails with HE.unknown(pg tries to utf8 encode)
    else H.encoderAndParam (HE.nullable HE.unknown) (toS <$> body) <> "::" <> H.sql typ

selectBody :: SqlFragment
selectBody = "(SELECT val FROM pgrst_body)"

pgFmtLit :: Text -> SqlFragment
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> T.replace "'" "''" trimmed <> "'"
     slashed = T.replace "\\" "\\\\" escaped in
 encodeUtf8 $ if "\\" `T.isInfixOf` escaped
   then "E" <> slashed
   else slashed

-- TODO: refactor by following https://github.com/PostgREST/postgrest/pull/1631#issuecomment-711070833
pgFmtIdent :: Text -> SqlFragment
pgFmtIdent x = encodeUtf8 $ "\"" <> T.replace "\"" "\"\"" (trimNullChars x) <> "\""

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')

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

asJsonSingleF :: Bool -> SqlFragment --TODO! unsafe when the query actually returns multiple rows, used only on inserting and returning single element
asJsonSingleF returnsScalar
  | returnsScalar = "coalesce(string_agg(to_json(_postgrest_t.pgrst_scalar)::text, ','), 'null')::character varying"
  | otherwise     = "coalesce(string_agg(to_json(_postgrest_t)::text, ','), '')::character varying"

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

pgFmtField :: QualifiedIdentifier -> Field -> H.Snippet
pgFmtField table (c, jp) = H.sql (pgFmtColumn table c) <> pgFmtJsonPath jp

pgFmtSelectItem :: QualifiedIdentifier -> SelectItem -> H.Snippet
pgFmtSelectItem table (f@(fName, jp), Nothing, alias, _) = pgFmtField table f <> H.sql (pgFmtAs fName jp alias)
-- Ideally we'd quote the cast with "pgFmtIdent cast". However, that would invalidate common casts such as "int", "bigint", etc.
-- Try doing: `select 1::"bigint"` - it'll err, using "int8" will work though. There's some parser magic that pg does that's invalidated when quoting.
-- Not quoting should be fine, we validate the input on Parsers.
pgFmtSelectItem table (f@(fName, jp), Just cast, alias, _) = "CAST (" <> pgFmtField table f <> " AS " <> H.sql (encodeUtf8 cast) <> " )" <> H.sql (pgFmtAs fName jp alias)

pgFmtOrderTerm :: QualifiedIdentifier -> OrderTerm -> H.Snippet
pgFmtOrderTerm qi ot =
  pgFmtField qi (otTerm ot) <> " " <>
  H.sql (BS.unwords [
    BS.pack $ maybe mempty show $ otDirection ot,
    BS.pack $ maybe mempty show $ otNullOrder ot])

pgFmtFilter :: QualifiedIdentifier -> Filter -> H.Snippet
pgFmtFilter table (Filter fld (OpExpr hasNot oper)) = notOp <> " " <> case oper of
   Op op val  -> pgFmtFieldOp op <> " " <> case op of
     "like"  -> unknownLiteral (T.map star val)
     "ilike" -> unknownLiteral (T.map star val)
     "is"    -> isAllowed val
     _       -> unknownLiteral val

   -- We don't use "IN", we use "= ANY". IN has the following disadvantages:
   -- + No way to use an empty value on IN: "col IN ()" is invalid syntax. With ANY we can do "= ANY('{}')"
   -- + Can invalidate prepared statements: multiple parameters on an IN($1, $2, $3) will lead to using different prepared statements and not take advantage of caching.
   In vals -> pgFmtField table fld <> " " <>
    case vals of
      [""] -> "= ANY('{}') "
      -- Here we build the pg array, e.g '{"Hebdon, John","Other","Another"}', manually. We quote the values to prevent the "," being treated as an element separator.
      -- TODO: Ideally this would be done on Hasql with an encoder, but the "array unknown" is not working(Hasql doesn't pass any value).
      _    -> "= ANY (" <> unknownLiteral ("{" <> T.intercalate "," ((\x -> "\"" <> x <> "\"") <$> vals) <> "}") <> ")"

   Fts op lang val ->
     pgFmtFieldOp op <> "(" <> ftsLang lang <> unknownLiteral val <> ") "
 where
   ftsLang = maybe mempty (\l -> unknownLiteral l <> ", ")
   pgFmtFieldOp op = pgFmtField table fld <> " " <> sqlOperator op
   sqlOperator o = H.sql $ HM.lookupDefault "=" o operators
   notOp = if hasNot then "NOT" else mempty
   star c = if c == '*' then '%' else c
   -- IS cannot be prepared. `PREPARE boolplan AS SELECT * FROM projects where id IS $1` will give a syntax error.
   -- The above can be fixed by using `PREPARE boolplan AS SELECT * FROM projects where id IS NOT DISTINCT FROM $1;`
   -- However that would not accept the TRUE/FALSE/NULL keywords. See: https://stackoverflow.com/questions/6133525/proper-way-to-set-preparedstatement-parameter-to-null-under-postgres.
   isAllowed :: Text -> H.Snippet
   isAllowed v = H.sql $ maybe
     (pgFmtLit v <> "::unknown") encodeUtf8
     (find ((==) . T.toLower $ v) ["null","true","false"])

pgFmtJoinCondition :: JoinCondition -> H.Snippet
pgFmtJoinCondition (JoinCondition (qi1, col1) (qi2, col2)) =
  H.sql $ pgFmtColumn qi1 col1 <> " = " <> pgFmtColumn qi2 col2

pgFmtLogicTree :: QualifiedIdentifier -> LogicTree -> H.Snippet
pgFmtLogicTree qi (Expr hasNot op forest) = H.sql notOp <> " (" <> intercalateSnippet (" " <> BS.pack (show op) <> " ") (pgFmtLogicTree qi <$> forest) <> ")"
  where notOp =  if hasNot then "NOT" else mempty
pgFmtLogicTree qi (Stmnt flt) = pgFmtFilter qi flt

pgFmtJsonPath :: JsonPath -> H.Snippet
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

countF :: H.Snippet -> Bool -> (H.Snippet, SqlFragment)
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

limitOffsetF :: NonnegRange -> H.Snippet
limitOffsetF range =
  if range == allRange then mempty else "LIMIT " <> limit <> " OFFSET " <> offset
  where
    limit = maybe "ALL" (\l -> unknownEncoder (BS.pack $ show l)) $ rangeLimit range
    offset = unknownEncoder (BS.pack . show $ rangeOffset range)

responseHeadersF :: PgVersion -> SqlFragment
responseHeadersF pgVer =
  if pgVer >= pgVersion96
    then currentSettingF "response.headers"
    else "null"

responseStatusF :: PgVersion -> SqlFragment
responseStatusF pgVer =
  if pgVer >= pgVersion96
    then currentSettingF "response.status"
    else "null"

currentSettingF :: Text -> SqlFragment
currentSettingF setting =
  -- nullif is used because of https://gist.github.com/steve-chavez/8d7033ea5655096903f3b52f8ed09a15
  "nullif(current_setting(" <> pgFmtLit setting <> ", true), '')"

-- Hasql Snippet utilities
unknownEncoder :: ByteString -> H.Snippet
unknownEncoder = H.encoderAndParam (HE.nonNullable HE.unknown)

unknownLiteral :: Text -> H.Snippet
unknownLiteral = unknownEncoder . encodeUtf8

intercalateSnippet :: ByteString -> [H.Snippet] -> H.Snippet
intercalateSnippet _ [] = mempty
intercalateSnippet frag snippets = foldr1 (\a b -> a <> H.sql frag <> b) snippets
