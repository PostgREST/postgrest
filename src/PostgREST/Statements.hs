{-|
Module      : PostgREST.Statements
Description : PostgREST single SQL statements.

This module constructs single SQL statements that can be parametrized and prepared.

- It consumes the SqlQuery types generated by the QueryBuilder module.
- It generates the body format and some headers of the final HTTP response.

TODO: Currently, createReadStatement is not using prepared statements. See https://github.com/PostgREST/postgrest/issues/718.
-}
module PostgREST.Statements (
    createWriteStatement
  , createReadStatement
  , callProcStatement
  , createExplainStatement
) where


import           Control.Lens                   ((^?))
import           Data.Aeson                     as JSON
import qualified Data.Aeson.Lens                as L
import qualified Data.ByteString.Char8          as BS
import           Data.Maybe
import           Data.Text                      (intercalate, unwords)
import           Data.Text.Encoding             (encodeUtf8)
import qualified Hasql.Decoders                 as HD
import qualified Hasql.Encoders                 as HE
import qualified Hasql.Statement                as H
import           PostgREST.ApiRequest           (PreferRepresentation (..))
import           PostgREST.QueryBuilder.Private
import           PostgREST.Types
import           Protolude                      hiding (cast,
                                                 intercalate, replace)
import           Text.InterpolatedString.Perl6  (qc)

{-| The generic query result format used by API responses. The location header
    is represented as a list of strings containing variable bindings like
    @"k1=eq.42"@, or the empty list if there is no location header.
-}
type ResultsWithCount = (Maybe Int64, Int64, [BS.ByteString], BS.ByteString)

createWriteStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool ->
                        PreferRepresentation -> [Text] ->
                        H.Statement ByteString ResultsWithCount
createWriteStatement selectQuery mutateQuery wantSingle isInsert asCsv rep pKeys =
  unicodeStatement sql (param HE.unknown) decodeStandard True
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

  decodeStandard :: HD.Result ResultsWithCount
  decodeStandard =
   fromMaybe (Nothing, 0, [], "") <$> HD.rowMaybe standardRow

createReadStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool -> Maybe FieldName ->
                       H.Statement () ResultsWithCount
createReadStatement selectQuery countQuery isSingle countTotal asCsv binaryField =
  unicodeStatement sql HE.noParams decodeStandard False
 where
  sql = [qc|
      WITH
      {sourceCTEName} AS ({selectQuery})
      {countCTEF}
      SELECT
        {countResultF} AS total_result_set,
        pg_catalog.count(_postgrest_t) AS page_total,
        {noLocationF} AS header,
        {bodyF} AS body
      FROM ( SELECT * FROM {sourceCTEName}) _postgrest_t |]

  (countCTEF, countResultF) = countF countQuery countTotal

  bodyF
    | asCsv = asCsvF
    | isSingle = asJsonSingleF
    | isJust binaryField = asBinaryF $ fromJust binaryField
    | otherwise = asJsonF

  decodeStandard :: HD.Result ResultsWithCount
  decodeStandard =
    HD.singleRow standardRow

{-| Read and Write api requests use a similar response format which includes
    various record counts and possible location header. This is the decoder
    for that common type of query.
-}
standardRow :: HD.Row ResultsWithCount
standardRow = (,,,) <$> nullableColumn HD.int8 <*> column HD.int8
                    <*> column header <*> column HD.bytea
  where
    header = HD.array $ HD.dimension replicateM $ element HD.bytea

type ProcResults = (Maybe Int64, Int64, ByteString, Either Text [GucHeader])

callProcStatement :: Bool -> SqlQuery -> SqlQuery -> SqlQuery -> Bool ->
                     Bool -> Bool -> Bool -> Bool -> Maybe FieldName -> PgVersion ->
                     H.Statement ByteString ProcResults
callProcStatement returnsScalar callProcQuery selectQuery countQuery countTotal isSingle asCsv asBinary multObjects binaryField pgVer =
  unicodeStatement sql (param HE.unknown) decodeProc True
  where
    sql = [qc|
      WITH {sourceCTEName} AS ({callProcQuery})
      {countCTEF}
      SELECT
        {countResultF} AS total_result_set,
        pg_catalog.count(_postgrest_t) AS page_total,
        {bodyF} AS body,
        {responseHeaders} AS response_headers
      FROM ({selectQuery}) _postgrest_t;|]

    (countCTEF, countResultF) = countF countQuery countTotal

    bodyF
     | returnsScalar = scalarBodyF
     | isSingle     = asJsonSingleF
     | asCsv = asCsvF
     | isJust binaryField = asBinaryF $ fromJust binaryField
     | otherwise = asJsonF

    scalarBodyF
     | asBinary    = asBinaryF "pgrst_scalar"
     | multObjects = "json_agg(_postgrest_t.pgrst_scalar)::character varying"
     | otherwise   = "(json_agg(_postgrest_t.pgrst_scalar)->0)::character varying"

    responseHeaders =
      if pgVer >= pgVersion96
        then "coalesce(nullif(current_setting('response.headers', true), ''), '[]')" :: Text -- nullif is used because of https://gist.github.com/steve-chavez/8d7033ea5655096903f3b52f8ed09a15
        else "'[]'" :: Text

    decodeProc :: HD.Result ProcResults
    decodeProc =
      let row = fromMaybe (Just 0, 0, "[]", "[]") <$> HD.rowMaybe procRow in
      (\(a, b, c, d) -> (a, b, c, first toS $ JSON.eitherDecode $ toS d)) <$> row
      where
        procRow = (,,,) <$> nullableColumn HD.int8 <*> column HD.int8
                        <*> column HD.bytea <*> column HD.bytea

createExplainStatement :: SqlQuery -> H.Statement () (Maybe Int64)
createExplainStatement countQuery =
  unicodeStatement sql HE.noParams decodeExplain False
  where
    sql = [qc| EXPLAIN (FORMAT JSON) {countQuery} |]
    -- |
    -- An `EXPLAIN (FORMAT JSON) select * from items;` output looks like this:
    -- [{
    --   "Plan": {
    --     "Node Type": "Seq Scan", "Parallel Aware": false, "Relation Name": "items",
    --     "Alias": "items", "Startup Cost": 0.00, "Total Cost": 32.60,
    --     "Plan Rows": 2260,"Plan Width": 8} }]
    -- We only obtain the Plan Rows here.
    decodeExplain :: HD.Result (Maybe Int64)
    decodeExplain =
      let row = HD.singleRow $ column HD.bytea in
      (^? L.nth 0 . L.key "Plan" .  L.key "Plan Rows" . L._Integral) <$> row

unicodeStatement :: Text -> HE.Params a -> HD.Result b -> Bool -> H.Statement a b
unicodeStatement = H.Statement . encodeUtf8

-- Helper hasql functions

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

element :: HD.Value a -> HD.Array a
element = HD.element . HD.nonNullable

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable
