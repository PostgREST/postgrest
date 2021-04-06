{-# LANGUAGE DuplicateRecordFields #-}
{-|
Module      : PostgREST.QueryBuilder
Description : PostgREST SQL queries generating functions.

This module provides functions to consume data types that
represent database queries (e.g. ReadRequest, MutateRequest) and SqlFragment
to produce SqlQuery type outputs.
-}
module PostgREST.QueryBuilder
  ( readRequestToQuery
  , mutateRequestToQuery
  , readRequestToCountQuery
  , requestToCallProcQuery
  , limitedQuery
  , setConfigLocal
  ) where

import qualified Data.ByteString.Char8           as BS
import qualified Data.Set                        as S
import qualified Hasql.DynamicStatements.Snippet as H

import Data.Tree (Tree (..))

import PostgREST.ApiRequest       (PayloadJSON (..))
import PostgREST.DbStructureTypes (Cardinality (..), FieldName,
                                   PgArg (..),
                                   QualifiedIdentifier (..),
                                   Relation (..), Table (..), qiName)
import PostgREST.Preferences      (PreferParameters (..),
                                   PreferResolution (..))

import PostgREST.Private.Common
import PostgREST.Private.QueryFragment
import PostgREST.Queries

import Protolude

readRequestToQuery :: ReadRequest -> H.Snippet
readRequestToQuery (Node (Select colSelects mainQi tblAlias implJoins logicForest joinConditions_ ordts range, _) forest) =
  "SELECT " <>
  intercalateSnippet ", " ((pgFmtSelectItem qi <$> colSelects) ++ selects) <>
  "FROM " <> H.sql (BS.intercalate ", " (tabl : implJs)) <> " " <>
  intercalateSnippet " " joins <> " " <>
  (if null logicForest && null joinConditions_ then mempty else "WHERE " <> intercalateSnippet " AND " (map (pgFmtLogicTree qi) logicForest ++ map pgFmtJoinCondition joinConditions_))
  <> " " <>
  (if null ordts then mempty else "ORDER BY " <> intercalateSnippet ", " (map (pgFmtOrderTerm qi) ordts)) <> " " <>
  limitOffsetF range
  where
    implJs = fromQi <$> implJoins
    tabl = fromQi mainQi <> maybe mempty (\a -> " AS " <> pgFmtIdent a) tblAlias
    qi = maybe mainQi (QualifiedIdentifier mempty) tblAlias
    (joins, selects) = foldr getJoinsSelects ([],[]) forest

getJoinsSelects :: ReadRequest -> ([H.Snippet], [H.Snippet]) -> ([H.Snippet], [H.Snippet])
getJoinsSelects rr@(Node (_, (name, Just Relation{relType=relTyp,relTable=Table{tableName=table}}, alias, _, _)) _) (j,s) =
  let subquery = readRequestToQuery rr in
  case relTyp of
    M2O ->
      let aliasOrName = fromMaybe name alias
          localTableName = pgFmtIdent $ table <> "_" <> aliasOrName
          sel = H.sql ("row_to_json(" <> localTableName <> ".*) AS " <> pgFmtIdent aliasOrName)
          joi = " LEFT JOIN LATERAL( " <> subquery <> " ) AS " <> H.sql localTableName <> " ON TRUE " in
      (joi:j,sel:s)
    _ ->
      let sel = "COALESCE (("
             <> "SELECT json_agg(" <> H.sql (pgFmtIdent table) <> ".*) "
             <> "FROM (" <> subquery <> ") " <> H.sql (pgFmtIdent table) <> " "
             <> "), '[]') AS " <> H.sql (pgFmtIdent (fromMaybe name alias)) in
      (j,sel:s)
getJoinsSelects (Node (_, (_, Nothing, _, _, _)) _) _ = ([], [])

mutateRequestToQuery :: MutateRequest -> H.Snippet
mutateRequestToQuery (Insert mainQi iCols body onConflct putConditions returnings) =
  "WITH " <> normalizedBody body <> " " <>
  "INSERT INTO " <> H.sql (fromQi mainQi) <> H.sql (if S.null iCols then " " else "(" <> cols <> ") ") <>
  "SELECT " <> H.sql cols <> " " <>
  H.sql ("FROM json_populate_recordset (null::" <> fromQi mainQi <> ", " <> selectBody <> ") _ ") <>
  -- Only used for PUT
  (if null putConditions then mempty else "WHERE " <> intercalateSnippet " AND " (pgFmtLogicTree (QualifiedIdentifier mempty "_") <$> putConditions)) <>
  H.sql (BS.unwords [
    maybe "" (\(oncDo, oncCols) ->
      if null oncCols then
        mempty
      else
        "ON CONFLICT(" <> BS.intercalate ", " (pgFmtIdent <$> oncCols) <> ") " <> case oncDo of
        IgnoreDuplicates ->
          "DO NOTHING"
        MergeDuplicates  ->
          if S.null iCols
             then "DO NOTHING"
             else "DO UPDATE SET " <> BS.intercalate ", " (pgFmtIdent <> const " = EXCLUDED." <> pgFmtIdent <$> S.toList iCols)
      ) onConflct,
    returningF mainQi returnings
    ])
  where
    cols = BS.intercalate ", " $ pgFmtIdent <$> S.toList iCols
mutateRequestToQuery (Update mainQi uCols body logicForest returnings) =
  if S.null uCols
    -- if there are no columns we cannot do UPDATE table SET {empty}, it'd be invalid syntax
    -- selecting an empty resultset from mainQi gives us the column names to prevent errors when using &select=
    -- the select has to be based on "returnings" to make computed overloaded functions not throw
    then H.sql ("SELECT " <> emptyBodyReturnedColumns <> " FROM " <> fromQi mainQi <> " WHERE false")
    else
      "WITH " <> normalizedBody body <> " " <>
      "UPDATE " <> H.sql (fromQi mainQi) <> " SET " <> H.sql cols <> " " <>
      "FROM (SELECT * FROM json_populate_recordset (null::" <> H.sql (fromQi mainQi) <> " , " <> H.sql selectBody <> " )) _ " <>
      (if null logicForest then mempty else "WHERE " <> intercalateSnippet " AND " (pgFmtLogicTree mainQi <$> logicForest)) <> " " <>
      H.sql (returningF mainQi returnings)
  where
    cols = BS.intercalate ", " (pgFmtIdent <> const " = _." <> pgFmtIdent <$> S.toList uCols)
    emptyBodyReturnedColumns :: SqlFragment
    emptyBodyReturnedColumns
      | null returnings = "NULL"
      | otherwise       = BS.intercalate ", " (pgFmtColumn (QualifiedIdentifier mempty $ qiName mainQi) <$> returnings)
mutateRequestToQuery (Delete mainQi logicForest returnings) =
  "DELETE FROM " <> H.sql (fromQi mainQi) <> " " <>
  (if null logicForest then mempty else "WHERE " <> intercalateSnippet " AND " (map (pgFmtLogicTree mainQi) logicForest)) <> " " <>
  H.sql (returningF mainQi returnings)

requestToCallProcQuery :: QualifiedIdentifier -> [PgArg] -> Maybe PayloadJSON -> Bool -> Maybe PreferParameters -> [FieldName] -> H.Snippet
requestToCallProcQuery qi pgArgs pj returnsScalar preferParams returnings =
  argsCTE <> sourceBody
  where
    body = pjRaw <$> pj
    paramsAsSingleObject    = preferParams == Just SingleObject
    paramsAsMultipleObjects = preferParams == Just MultipleObjects

    (argsCTE, args)
      | null pgArgs = (mempty, mempty)
      | paramsAsSingleObject = ("WITH pgrst_args AS (SELECT NULL)", jsonPlaceHolder body)
      | otherwise = (
          "WITH " <> normalizedBody body <> ", " <>
          H.sql (
            BS.unwords [
            "pgrst_args AS (",
              "SELECT * FROM json_to_recordset(" <> selectBody <> ") AS _(" <> fmtArgs (const mempty) (\a -> " " <> encodeUtf8 (pgaType a)) <> ")",
            ")"])
         , H.sql $ if paramsAsMultipleObjects
             then fmtArgs varadicPrefix (\a -> " := pgrst_args." <> pgFmtIdent (pgaName a))
             else fmtArgs varadicPrefix (\a -> " := (SELECT " <> pgFmtIdent (pgaName a) <> " FROM pgrst_args LIMIT 1)")
        )

    fmtArgs :: (PgArg -> SqlFragment) -> (PgArg -> SqlFragment) -> SqlFragment
    fmtArgs argFragPre argFragSuf = BS.intercalate ", " ((\a -> argFragPre a <> pgFmtIdent (pgaName a) <> argFragSuf a) <$> pgArgs)

    varadicPrefix :: PgArg -> SqlFragment
    varadicPrefix a = if pgaVar a then "VARIADIC " else mempty

    sourceBody :: H.Snippet
    sourceBody
      | paramsAsMultipleObjects =
          if returnsScalar
            then "SELECT " <> callIt <> " AS pgrst_scalar FROM pgrst_args"
            else "SELECT pgrst_lat_args.* FROM pgrst_args, " <>
                 "LATERAL ( SELECT " <> returnedColumns <> " FROM " <> callIt <> " ) pgrst_lat_args"
      | otherwise =
          if returnsScalar
            then "SELECT " <> callIt <> " AS pgrst_scalar"
            else "SELECT " <> returnedColumns <> " FROM " <> callIt

    callIt :: H.Snippet
    callIt = H.sql (fromQi qi) <> "(" <> args <> ")"

    returnedColumns :: H.Snippet
    returnedColumns
      | null returnings = "*"
      | otherwise       = H.sql $ BS.intercalate ", " (pgFmtColumn (QualifiedIdentifier mempty $ qiName qi) <$> returnings)


-- | SQL query meant for COUNTing the root node of the Tree.
-- It only takes WHERE into account and doesn't include LIMIT/OFFSET because it would reduce the COUNT.
-- SELECT 1 is done instead of SELECT * to prevent doing expensive operations(like functions based on the columns)
-- inside the FROM target.
readRequestToCountQuery :: ReadRequest -> H.Snippet
readRequestToCountQuery (Node (Select{from=qi, where_=logicForest}, _) _) =
 "SELECT 1 " <> "FROM " <> H.sql (fromQi qi) <> " " <>
 if null logicForest then mempty else "WHERE " <> intercalateSnippet " AND " (map (pgFmtLogicTree qi) logicForest)

limitedQuery :: H.Snippet -> Maybe Integer -> H.Snippet
limitedQuery query maxRows = query <> H.sql (maybe mempty (\x -> " LIMIT " <> BS.pack (show x)) maxRows)

-- | Do a pg set_config(setting, value, true) call. This is equivalent to a SET LOCAL.
setConfigLocal :: Text -> (Text, Text) -> H.Snippet
setConfigLocal prefix (k, v) =
  "set_config(" <> unknownLiteral (prefix <> k) <> ", " <> unknownLiteral v <> ", true)"
