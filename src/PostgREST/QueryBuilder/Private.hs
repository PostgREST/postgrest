{-# LANGUAGE LambdaCase #-}
{-|
Module      : PostgREST.QueryBuilder.Private
Description : Helper functions for PostgREST.QueryBuilder.
-}
module PostgREST.QueryBuilder.Private where

import qualified Data.Aeson                    as JSON
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe
import           Data.Scientific               (FPFormat (..),
                                                formatScientific,
                                                isInteger)
import           Data.Text                     (intercalate,
                                                isInfixOf, replace,
                                                toLower, unwords)
import qualified Data.Text                     as T (map, null,
                                                     takeWhile)
import           PostgREST.Types
import           Protolude                     hiding (cast,
                                                intercalate, replace)
import           Text.InterpolatedString.Perl6 (qc)

noLocationF :: Text
noLocationF = "array[]::text[]"

removeSourceCTESchema :: Schema -> TableName -> QualifiedIdentifier
removeSourceCTESchema schema tbl = QualifiedIdentifier (if tbl == sourceCTEName then "" else schema) tbl

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

pgFmtLit :: SqlFragment -> SqlFragment
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> replace "'" "''" trimmed <> "'"
     slashed = replace "\\" "\\\\" escaped in
 if "\\" `isInfixOf` escaped
   then "E" <> slashed
   else slashed

pgFmtIdent :: SqlFragment -> SqlFragment
pgFmtIdent x = "\"" <> replace "\"" "\"\"" (trimNullChars $ toS x) <> "\""

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

emptyOnFalse :: Text -> Bool -> Text
emptyOnFalse val cond = if cond then "" else val

pgFmtColumn :: QualifiedIdentifier -> Text -> SqlFragment
pgFmtColumn table "*" = fromQi table <> ".*"
pgFmtColumn table c   = fromQi table <> "." <> pgFmtIdent c

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
     "like"  -> unknownLiteral (T.map star val)
     "ilike" -> unknownLiteral (T.map star val)
     "is"    -> whiteList val
     _       -> unknownLiteral val

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

unquoted :: JSON.Value -> Text
unquoted (JSON.String t) = t
unquoted (JSON.Number n) =
  toS $ formatScientific Fixed (if isInteger n then Just 0 else Nothing) n
unquoted (JSON.Bool b) = show b
unquoted v = toS $ JSON.encode v
