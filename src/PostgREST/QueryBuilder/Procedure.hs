module PostgREST.QueryBuilder.Procedure where

import           Data.Maybe
import           Data.Text                      (intercalate, unwords)
import qualified Hasql.Decoders                 as HD
import qualified Hasql.Encoders                 as HE
import qualified Hasql.Statement                as H
import           PostgREST.QueryBuilder.Private
import           PostgREST.Types
import           Protolude                      hiding (cast,
                                                 intercalate, replace)
import           Text.InterpolatedString.Perl6  (qc)

type ProcResults = (Maybe Int64, Int64, ByteString, ByteString)
callProc :: QualifiedIdentifier -> [PgArg] -> Bool -> SqlQuery -> SqlQuery -> Bool ->
            Bool -> Bool -> Bool -> Bool -> Maybe FieldName -> PgVersion ->
            H.Statement ByteString (Maybe ProcResults)
callProc qi pgArgs returnsScalar selectQuery countQuery countTotal isSingle paramsAsSingleObject asCsv asBinary binaryField pgVer =
  unicodeStatement sql (param HE.unknown) decodeProc True
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
    procRow = (,,,) <$> nullableColumn HD.int8 <*> column HD.int8
                    <*> column HD.bytea <*> column HD.bytea

