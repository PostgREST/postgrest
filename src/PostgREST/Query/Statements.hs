{-|
Module      : PostgREST.Query.Statements
Description : PostgREST SQL statements.
-}
module PostgREST.Query.Statements
  ( prepareWrite
  , prepareRead
  , prepareCall
  , preparePlanRows
  ) where

import qualified Hasql.DynamicStatements.Snippet as SQL

import PostgREST.ApiRequest.Preferences
import PostgREST.MediaType              (MTVndPlanFormat (..),
                                         MediaType (..))
import PostgREST.Query.SqlFragment
import PostgREST.SchemaCache.Routine    (MediaHandler (..), Routine,
                                         funcReturnsSingle)

import Protolude

prepareWrite :: SQL.Snippet -> SQL.Snippet -> Bool -> Bool -> MediaType -> MediaHandler ->
                Maybe PreferRepresentation -> Maybe PreferResolution -> [Text] -> SQL.Snippet
prepareWrite selectQuery mutateQuery isInsert isPut mt handler rep resolution pKeys = mtSnippet mt snippet
 where
  checkUpsert snip = if isInsert && (isPut || resolution == Just MergeDuplicates) then snip else "''"
  pgrstInsertedF = checkUpsert "nullif(current_setting('pgrst.inserted', true),'')::int"
  snippet =
    "WITH " <> sourceCTE <> " AS (" <> mutateQuery <> ") " <>
    "SELECT " <>
      "'' AS total_result_set, " <>
      "pg_catalog.count(_postgrest_t) AS page_total, " <>
      locF <> " AS header, " <>
      handlerF Nothing handler <> " AS body, " <>
      responseHeadersF <> " AS response_headers, " <>
      responseStatusF  <> " AS response_status, " <>
      pgrstInsertedF <> " AS response_inserted " <>
    "FROM (" <> selectF <> ") _postgrest_t"

  locF =
    if isInsert && rep == Just HeadersOnly
      then
        "CASE WHEN pg_catalog.count(_postgrest_t) = 1 " <>
          "THEN coalesce(" <> locationF pKeys <> ", " <> noLocationF <> ") " <>
          "ELSE " <> noLocationF <> " " <>
        "END"
      else noLocationF

  selectF
    -- prevent using any of the column names in ?select= when no response is returned from the CTE
    | handler == NoAgg = "SELECT * FROM " <> sourceCTE
    | otherwise        = selectQuery

prepareRead :: SQL.Snippet -> SQL.Snippet -> Bool -> MediaType -> MediaHandler -> SQL.Snippet
prepareRead selectQuery countQuery countTotal mt handler = mtSnippet mt snippet
 where
  snippet =
    "WITH " <> sourceCTE <> " AS ( " <> selectQuery <> " ) " <>
    countCTEF <> " " <>
    "SELECT " <>
      countResultF <> " AS total_result_set, " <>
      "pg_catalog.count(_postgrest_t) AS page_total, " <>
      handlerF Nothing handler <> " AS body, " <>
      responseHeadersF <> " AS response_headers, " <>
      responseStatusF <> " AS response_status, " <>
      "''" <> " AS response_inserted " <>
    "FROM ( SELECT * FROM " <> sourceCTE <> " ) _postgrest_t"

  (countCTEF, countResultF) = countF countQuery countTotal


prepareCall :: Routine -> SQL.Snippet -> SQL.Snippet -> SQL.Snippet -> Bool ->
               MediaType -> MediaHandler -> SQL.Snippet
prepareCall rout callProcQuery selectQuery countQuery countTotal mt handler = mtSnippet mt snippet
  where
    snippet =
      "WITH " <> sourceCTE <> " AS (" <> callProcQuery <> ") " <>
      countCTEF <>
      "SELECT " <>
        countResultF <> " AS total_result_set, " <>
        (if funcReturnsSingle rout
          then "1"
          else "pg_catalog.count(_postgrest_t)") <> " AS page_total, " <>
        handlerF (Just rout) handler <> " AS body, " <>
        responseHeadersF <> " AS response_headers, " <>
        responseStatusF <> " AS response_status, " <>
        "''" <> " AS response_inserted " <>
      "FROM (" <> selectQuery <> ") _postgrest_t"

    (countCTEF, countResultF) = countF countQuery countTotal


preparePlanRows :: SQL.Snippet -> SQL.Snippet
preparePlanRows = explainF PlanJSON mempty

mtSnippet :: MediaType -> SQL.Snippet -> SQL.Snippet
mtSnippet mediaType snippet = case mediaType of
  MTVndPlan _ fmt opts -> explainF fmt opts snippet
  _                    -> snippet
