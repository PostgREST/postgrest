module PostgREST.QueryBuilder.WriteStatement where

import qualified Hasql.Encoders        as HE
import qualified Hasql.Statement       as H
import Data.Text                     (intercalate, unwords)
import Data.Maybe
import Text.InterpolatedString.Perl6 (qc)
import PostgREST.Types
import PostgREST.QueryBuilder.Private
import PostgREST.ApiRequest (PreferRepresentation (..))
import Protolude            hiding (cast, intercalate, replace)

createWriteStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool ->
                        PreferRepresentation -> [Text] ->
                        H.Statement ByteString (Maybe ResultsWithCount)
createWriteStatement selectQuery mutateQuery wantSingle isInsert asCsv rep pKeys =
  unicodeStatement sql (param HE.unknown) decodeStandardMay True

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
