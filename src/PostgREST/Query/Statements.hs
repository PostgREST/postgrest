{-|
Module      : PostgREST.Query.Statements
Description : PostgREST single SQL statements.

This module constructs single SQL statements that can be parametrized and prepared.

- It consumes the SqlQuery types generated by the QueryBuilder module.
- It generates the body format and some headers of the final HTTP response.
-}
module PostgREST.Query.Statements
  ( prepareWrite
  , prepareRead
  , prepareCall
  , preparePlanRows
  , ResultSet (..)
  ) where

import qualified Data.Aeson.Lens                   as L
import qualified Data.ByteString.Char8             as BS
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Snippet   as SQL
import qualified Hasql.DynamicStatements.Statement as SQL
import qualified Hasql.Statement                   as SQL

import Control.Lens ((^?))
import Data.Maybe   (fromJust)

import PostgREST.ApiRequest.Preferences
import PostgREST.DbStructure.Identifiers (FieldName)
import PostgREST.MediaType               (MTPlanAttrs (..),
                                          MTPlanFormat (..),
                                          MediaType (..),
                                          getMediaType)
import PostgREST.Query.SqlFragment

import Protolude

-- | Standard result set format used for all queries
data ResultSet
  = RSStandard
  { rsTableTotal :: Maybe Int64
  -- ^ count of all the table rows
  , rsQueryTotal :: Int64
  -- ^ count of the query rows
  , rsLocation   :: [(BS.ByteString, BS.ByteString)]
  -- ^ The Location header(only used for inserts) is represented as a list of strings containing
  -- variable bindings like @"k1=eq.42"@, or the empty list if there is no location header.
  , rsBody       :: BS.ByteString
  -- ^ the aggregated body of the query
  , rsGucHeaders :: Maybe BS.ByteString
  -- ^ the HTTP headers to be added to the response
  , rsGucStatus  :: Maybe Text
  -- ^ the HTTP status to be added to the response
  }
  | RSPlan BS.ByteString -- ^ the plan of the query


prepareWrite :: SQL.Snippet -> SQL.Snippet -> Bool -> MediaType ->
                PreferRepresentation -> [Text] -> Bool -> SQL.Statement () ResultSet
prepareWrite selectQuery mutateQuery isInsert mt rep pKeys =
  SQL.dynamicallyParameterized (mtSnippet mt snippet) decodeIt
 where
  snippet =
    "WITH " <> SQL.sql sourceCTEName <> " AS (" <> mutateQuery <> ") " <>
    SQL.sql (
    "SELECT " <>
      "'' AS total_result_set, " <>
      "pg_catalog.count(_postgrest_t) AS page_total, " <>
      locF <> " AS header, " <>
      bodyF <> " AS body, " <>
      responseHeadersF <> " AS response_headers, " <>
      responseStatusF  <> " AS response_status "
    ) <>
    "FROM (" <> selectF <> ") _postgrest_t"

  locF =
    if isInsert && rep == HeadersOnly
      then BS.unwords [
        "CASE WHEN pg_catalog.count(_postgrest_t) = 1",
          "THEN coalesce(" <> locationF pKeys <> ", " <> noLocationF <> ")",
          "ELSE " <> noLocationF,
        "END"]
      else noLocationF

  bodyF
    | rep /= Full                       = "''"
    | getMediaType mt == MTTextCSV      = asCsvF
    | getMediaType mt == MTGeoJSON      = asGeoJsonF
    | getMediaType mt == MTSingularJSON = asJsonSingleF False
    | otherwise                         = asJsonF False

  selectF
    -- prevent using any of the column names in ?select= when no response is returned from the CTE
    | rep /= Full = SQL.sql ("SELECT * FROM " <> sourceCTEName)
    | otherwise   = selectQuery

  decodeIt :: HD.Result ResultSet
  decodeIt = case mt of
    MTPlan{} -> planRow
    _        -> fromMaybe (RSStandard Nothing 0 mempty mempty Nothing Nothing) <$> HD.rowMaybe (standardRow False)

prepareRead :: SQL.Snippet -> SQL.Snippet -> Bool -> MediaType -> Maybe FieldName -> Bool -> SQL.Statement () ResultSet
prepareRead selectQuery countQuery countTotal mt binaryField =
  SQL.dynamicallyParameterized (mtSnippet mt snippet) decodeIt
 where
  snippet =
    "WITH " <>
    SQL.sql sourceCTEName <> " AS ( " <> selectQuery <> " ) " <>
    countCTEF <> " " <>
    SQL.sql ("SELECT " <>
      countResultF <> " AS total_result_set, " <>
      "pg_catalog.count(_postgrest_t) AS page_total, " <>
      bodyF <> " AS body, " <>
      responseHeadersF <> " AS response_headers, " <>
      responseStatusF <> " AS response_status " <>
    "FROM ( SELECT * FROM " <> sourceCTEName <> " ) _postgrest_t")

  (countCTEF, countResultF) = countF countQuery countTotal

  bodyF
    | getMediaType mt == MTTextCSV                       = asCsvF
    | getMediaType mt == MTSingularJSON                  = asJsonSingleF False
    | getMediaType mt == MTGeoJSON                       = asGeoJsonF
    | isJust binaryField && getMediaType mt == MTTextXML = asXmlF $ fromJust binaryField
    | isJust binaryField                                 = asBinaryF $ fromJust binaryField
    | otherwise                                          = asJsonF False

  decodeIt :: HD.Result ResultSet
  decodeIt = case mt of
    MTPlan{} -> planRow
    _        -> HD.singleRow $ standardRow True

prepareCall :: Bool -> Bool -> SQL.Snippet -> SQL.Snippet -> SQL.Snippet -> Bool ->
               MediaType -> Bool -> Maybe FieldName -> Bool ->
               SQL.Statement () ResultSet
prepareCall returnsScalar returnsSingle callProcQuery selectQuery countQuery countTotal mt multObjects binaryField =
  SQL.dynamicallyParameterized (mtSnippet mt snippet) decodeIt
  where
    snippet =
      "WITH " <> SQL.sql sourceCTEName <> " AS (" <> callProcQuery <> ") " <>
      countCTEF <>
      SQL.sql (
      "SELECT " <>
        countResultF <> " AS total_result_set, " <>
        "pg_catalog.count(_postgrest_t) AS page_total, " <>
        bodyF <> " AS body, " <>
        responseHeadersF <> " AS response_headers, " <>
        responseStatusF <> " AS response_status ") <>
      "FROM (" <> selectQuery <> ") _postgrest_t"

    (countCTEF, countResultF) = countF countQuery countTotal

    bodyF
     | getMediaType mt == MTSingularJSON                  = asJsonSingleF returnsScalar
     | getMediaType mt == MTTextCSV                       = asCsvF
     | getMediaType mt == MTGeoJSON                       = asGeoJsonF
     | isJust binaryField && getMediaType mt == MTTextXML = asXmlF $ fromJust binaryField
     | isJust binaryField                                 = asBinaryF $ fromJust binaryField
     | returnsSingle && not multObjects                   = asJsonSingleF returnsScalar
     | otherwise                                          = asJsonF returnsScalar

    decodeIt :: HD.Result ResultSet
    decodeIt = case mt of
      MTPlan{} -> planRow
      _        -> fromMaybe (RSStandard (Just 0) 0 mempty mempty Nothing Nothing) <$> HD.rowMaybe (standardRow True)

preparePlanRows :: SQL.Snippet -> Bool -> SQL.Statement () (Maybe Int64)
preparePlanRows countQuery =
  SQL.dynamicallyParameterized snippet decodeIt
  where
    snippet = explainF PlanJSON mempty countQuery
    decodeIt :: HD.Result (Maybe Int64)
    decodeIt =
      let row = HD.singleRow $ column HD.bytea in
      (^? L.nth 0 . L.key "Plan" .  L.key "Plan Rows" . L._Integral) <$> row

standardRow :: Bool -> HD.Row ResultSet
standardRow noLocation =
  RSStandard <$> nullableColumn HD.int8 <*> column HD.int8
             <*> (if noLocation then pure mempty else fmap splitKeyValue <$> arrayColumn HD.bytea) <*> column HD.bytea
             <*> nullableColumn HD.bytea
             <*> nullableColumn HD.text
  where
    splitKeyValue :: ByteString -> (ByteString, ByteString)
    splitKeyValue kv =
      let (k, v) = BS.break (== '=') kv in
      (k, BS.tail v)

mtSnippet :: MediaType -> SQL.Snippet -> SQL.Snippet
mtSnippet mediaType snippet = case mediaType of
  MTPlan (MTPlanAttrs _ fmt opts) -> explainF fmt opts snippet
  _                               -> snippet

-- | We use rowList because when doing EXPLAIN (FORMAT TEXT), the result comes as many rows. FORMAT JSON comes as one.
planRow :: HD.Result ResultSet
planRow = RSPlan . BS.unlines <$> HD.rowList (column HD.bytea)

column :: HD.Value a -> HD.Row a
column = HD.column . HD.nonNullable

nullableColumn :: HD.Value a -> HD.Row (Maybe a)
nullableColumn = HD.column . HD.nullable

arrayColumn :: HD.Value a -> HD.Row [a]
arrayColumn = column . HD.listArray . HD.nonNullable
