module PostgREST.QueryBuilder.ReadStatement where

import           Data.Maybe
import           Data.Text                      (intercalate)
import qualified Hasql.Encoders                 as HE
import qualified Hasql.Statement                as H
import           PostgREST.QueryBuilder.Private
import           PostgREST.Types
import           Protolude                      hiding (cast,
                                                 intercalate, replace)
import           Text.InterpolatedString.Perl6  (qc)

createReadStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool -> Maybe FieldName ->
                       H.Statement () ResultsWithCount
createReadStatement selectQuery countQuery isSingle countTotal asCsv binaryField =
  unicodeStatement sql HE.noParams decodeStandard False
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
