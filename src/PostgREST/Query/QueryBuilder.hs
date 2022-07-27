{-# LANGUAGE DuplicateRecordFields #-}
{-|
Module      : PostgREST.Query.QueryBuilder
Description : PostgREST SQL queries generating functions.

This module provides functions to consume data types that
represent database queries (e.g. ReadRequest, MutateRequest) and SqlFragment
to produce SqlQuery type outputs.
-}
module PostgREST.Query.QueryBuilder
  ( readRequestToQuery
  , mutateRequestToQuery
  , readRequestToCountQuery
  , requestToCallProcQuery
  , limitedQuery
  ) where

import qualified Data.ByteString.Char8           as BS
import qualified Data.Set                        as S
import qualified Hasql.DynamicStatements.Snippet as SQL

import Data.Tree (Tree (..))

import PostgREST.DbStructure.Identifiers  (QualifiedIdentifier (..))
import PostgREST.DbStructure.Proc         (ProcParam (..))
import PostgREST.DbStructure.Relationship (Cardinality (..),
                                           Relationship (..))
import PostgREST.Request.Preferences      (PreferResolution (..))

import PostgREST.Query.SqlFragment
import PostgREST.RangeQuery          (allRange)
import PostgREST.Request.MutateQuery
import PostgREST.Request.ReadQuery
import PostgREST.Request.Types

import Protolude

readRequestToQuery :: ReadRequest -> SQL.Snippet
readRequestToQuery (Node (Select colSelects mainQi tblAlias implJoins logicForest joinConditions_ ordts range, _) forest) =
  "SELECT " <>
  intercalateSnippet ", " ((pgFmtSelectItem qi <$> colSelects) ++ selects) <> " " <>
  "FROM " <> SQL.sql (BS.intercalate ", " (tabl : implJs)) <> " " <>
  intercalateSnippet " " joins <> " " <>
  (if null logicForest && null joinConditions_
    then mempty
    else "WHERE " <> intercalateSnippet " AND " (map (pgFmtLogicTree qi) logicForest ++ map pgFmtJoinCondition joinConditions_)) <> " " <>
  orderF qi ordts <> " " <>
  limitOffsetF range
  where
    implJs = fromQi <$> implJoins
    tabl = fromQi mainQi <> maybe mempty (\a -> " AS " <> pgFmtIdent a) tblAlias
    qi = maybe mainQi (QualifiedIdentifier mempty) tblAlias
    (selects, joins) = foldr getSelectsJoins ([],[]) forest

getSelectsJoins :: ReadRequest -> ([SQL.Snippet], [SQL.Snippet]) -> ([SQL.Snippet], [SQL.Snippet])
getSelectsJoins rr@(Node (_, (name, Just Relationship{relCardinality=card,relTable=QualifiedIdentifier{qiName=table}}, alias, _, joinType, _)) _) (selects,joins) =
  let
    subquery = readRequestToQuery rr
    aliasOrName = fromMaybe name alias
    locTblName = table <> "_" <> aliasOrName
    localTableName = pgFmtIdent locTblName
    internalTableName = pgFmtIdent $ "_" <> locTblName
    correlatedSubquery sub al cond =
      (if joinType == Just JTInner then "INNER" else "LEFT") <> " JOIN LATERAL ( " <> sub <> " ) AS " <> SQL.sql al <> " ON " <> cond
    (sel, joi) = case card of
      M2O _ _ ->
        ( SQL.sql ("row_to_json(" <> localTableName <> ".*) AS " <> pgFmtIdent aliasOrName)
        , correlatedSubquery subquery localTableName "TRUE")
      _ ->
        ( SQL.sql $ "COALESCE( " <> localTableName <> "." <> internalTableName <> ", '[]') AS " <> pgFmtIdent aliasOrName
        , correlatedSubquery (
            "SELECT json_agg(" <> SQL.sql internalTableName <> ") AS " <> SQL.sql internalTableName <>
            "FROM (" <> subquery <> " ) AS " <> SQL.sql internalTableName
          ) localTableName $ if joinType == Just JTInner then SQL.sql localTableName <> " IS NOT NULL" else "TRUE")
  in
  (sel:selects, joi:joins)
getSelectsJoins (Node (_, (_, Nothing, _, _, _, _)) _) _ = ([], [])

mutateRequestToQuery :: MutateRequest -> SQL.Snippet
mutateRequestToQuery (Insert mainQi iCols body onConflct putConditions returnings) =
  "WITH " <> normalizedBody body <> " " <>
  "INSERT INTO " <> SQL.sql (fromQi mainQi) <> SQL.sql (if S.null iCols then " " else "(" <> cols <> ") ") <>
  "SELECT " <> SQL.sql cols <> " " <>
  SQL.sql ("FROM json_populate_recordset (null::" <> fromQi mainQi <> ", " <> selectBody <> ") _ ") <>
  -- Only used for PUT
  (if null putConditions then mempty else "WHERE " <> intercalateSnippet " AND " (pgFmtLogicTree (QualifiedIdentifier mempty "_") <$> putConditions)) <>
  SQL.sql (BS.unwords [
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

-- An update without a limit is always filtered with a WHERE
mutateRequestToQuery (Update mainQi uCols body logicForest pkFlts range ordts returnings)
  | S.null uCols =
    -- if there are no columns we cannot do UPDATE table SET {empty}, it'd be invalid syntax
    -- selecting an empty resultset from mainQi gives us the column names to prevent errors when using &select=
    -- the select has to be based on "returnings" to make computed overloaded functions not throw
    SQL.sql $ "SELECT " <> emptyBodyReturnedColumns <> " FROM " <> fromQi mainQi <> " WHERE false"

  | range == allRange =
    let whereLogic | null logicForest = if null pkFlts then "FALSE" else pgrstUpdateBodyF
                   | otherwise        = logicForestF in
    "WITH " <> normalizedBody body <> " " <>
    "UPDATE " <> mainTbl <> " SET " <> SQL.sql nonRangeCols <> " " <>
    "FROM (SELECT * FROM json_populate_recordset (null::" <> mainTbl <> " , " <> SQL.sql selectBody <> " )) pgrst_update_body " <>
    "WHERE " <> whereLogic <> " " <>
    SQL.sql (returningF mainQi returnings)

  | otherwise =
    let whereLogic | null logicForest = mempty
                   | otherwise        = " WHERE " <> logicForestF in
    "WITH " <> normalizedBody body <> ", " <>
    "pgrst_update_body AS (SELECT * FROM json_populate_recordset (null::" <> mainTbl <> " , " <> SQL.sql selectBody <> " ) LIMIT 1), " <>
    "pgrst_affected_rows AS (" <>
      "SELECT " <> SQL.sql rangeIdF <> " FROM " <> mainTbl <> " " <>
      whereLogic <> " " <>
      orderF mainQi ordts <> " " <>
      limitOffsetF range <>
    ") " <>
    "UPDATE " <> mainTbl <> " SET " <> SQL.sql rangeCols <>
    "FROM pgrst_affected_rows " <>
    "WHERE " <> SQL.sql whereRangeIdF <> " " <>
    SQL.sql (returningF mainQi returnings)

  where
    mainTbl = SQL.sql (fromQi mainQi)
    logicForestF = intercalateSnippet " AND " (pgFmtLogicTree mainQi <$> logicForest)
    pgrstUpdateBodyF = SQL.sql (BS.intercalate " AND " $ (\x -> pgFmtColumn mainQi x <> " = " <> pgFmtColumn (QualifiedIdentifier mempty "pgrst_update_body") x) <$> pkFlts)
    emptyBodyReturnedColumns = if null returnings then "NULL" else BS.intercalate ", " (pgFmtColumn (QualifiedIdentifier mempty $ qiName mainQi) <$> returnings)
    nonRangeCols = BS.intercalate ", " (pgFmtIdent <> const " = pgrst_update_body." <> pgFmtIdent <$> S.toList uCols)
    rangeCols = BS.intercalate ", " ((\col -> pgFmtIdent col <> " = (SELECT " <> pgFmtIdent col <> " FROM pgrst_update_body) ") <$> S.toList uCols)
    (whereRangeIdF, rangeIdF) = mutRangeF mainQi (fst . otTerm <$> ordts)

mutateRequestToQuery (Delete mainQi dCols body logicForest range ordts returnings)
  -- The body is not mandatory, although a delete without filters is not possible.
  | range == allRange =
    let whereLogic | null logicForest = if hasEmptyBody || S.null dCols then "FALSE" else pgrstDeleteBodyF
                   | otherwise        = logicForestF in
    (if not (null logicForest) || hasEmptyBody
      then mempty
      else "WITH " <> normalizedBody body <> " ") <>
    "DELETE FROM " <> SQL.sql (fromQi mainQi) <> " " <>
    (if not (null logicForest) || hasEmptyBody
      then mempty
      else "USING (SELECT * FROM json_populate_recordset (null::" <> SQL.sql (fromQi mainQi) <> " , " <> SQL.sql selectBody <> " )) pgrst_delete_body ") <>
    "WHERE " <> whereLogic <> " " <>
    SQL.sql (returningF mainQi returnings)

  -- When using limits, the payload is ignored.
  | otherwise =
    let whereLogic | null logicForest = mempty
                   | otherwise        = " WHERE " <> logicForestF in
    "WITH " <>
    "pgrst_affected_rows AS (" <>
      "SELECT " <> SQL.sql rangeIdF <> " FROM " <> SQL.sql (fromQi mainQi) <>
       whereLogic <> " " <>
      orderF mainQi ordts <> " " <>
      limitOffsetF range <>
    ") " <>
    "DELETE FROM " <> SQL.sql (fromQi mainQi) <> " " <>
    "USING pgrst_affected_rows " <>
    "WHERE " <> SQL.sql whereRangeIdF <> " " <>
    SQL.sql (returningF mainQi returnings)

  where
    hasEmptyBody = body == mempty
    logicForestF = intercalateSnippet " AND " (pgFmtLogicTree mainQi <$> logicForest)
    pgrstDeleteBodyF = SQL.sql (BS.intercalate " AND " $ (\x -> pgFmtColumn mainQi x <> " = " <> pgFmtColumn (QualifiedIdentifier mempty "pgrst_delete_body") x) <$> S.toList dCols)
    (whereRangeIdF, rangeIdF) = mutRangeF mainQi (fst . otTerm <$> ordts)

requestToCallProcQuery :: CallRequest -> SQL.Snippet
requestToCallProcQuery (FunctionCall qi params args returnsScalar multipleCall returnings) =
  prmsCTE <> argsBody
  where
    (prmsCTE, argFrag) = case params of
      OnePosParam prm -> ("WITH pgrst_args AS (SELECT NULL)", singleParameter args (encodeUtf8 $ ppType prm))
      KeyParams []    -> (mempty, mempty)
      KeyParams prms  -> (
          "WITH " <> normalizedBody args <> ", " <>
          SQL.sql (
            BS.unwords [
            "pgrst_args AS (",
              "SELECT * FROM json_to_recordset(" <> selectBody <> ") AS _(" <> fmtParams prms (const mempty) (\a -> " " <> encodeUtf8 (ppType a)) <> ")",
            ")"])
         , SQL.sql $ if multipleCall
             then fmtParams prms varadicPrefix (\a -> " := pgrst_args." <> pgFmtIdent (ppName a))
             else fmtParams prms varadicPrefix (\a -> " := (SELECT " <> pgFmtIdent (ppName a) <> " FROM pgrst_args LIMIT 1)")
        )

    fmtParams :: [ProcParam] -> (ProcParam -> SqlFragment) -> (ProcParam -> SqlFragment) -> SqlFragment
    fmtParams prms prmFragPre prmFragSuf = BS.intercalate ", "
      ((\a -> prmFragPre a <> pgFmtIdent (ppName a) <> prmFragSuf a) <$> prms)

    varadicPrefix :: ProcParam -> SqlFragment
    varadicPrefix a = if ppVar a then "VARIADIC " else mempty

    argsBody :: SQL.Snippet
    argsBody
      | multipleCall =
          if returnsScalar
            then "SELECT " <> callIt <> " AS pgrst_scalar FROM pgrst_args"
            else "SELECT pgrst_lat_args.* FROM pgrst_args, " <>
                 "LATERAL ( SELECT " <> returnedColumns <> " FROM " <> callIt <> " ) pgrst_lat_args"
      | otherwise =
          if returnsScalar
            then "SELECT " <> callIt <> " AS pgrst_scalar"
            else "SELECT " <> returnedColumns <> " FROM " <> callIt

    callIt :: SQL.Snippet
    callIt = SQL.sql (fromQi qi) <> "(" <> argFrag <> ")"

    returnedColumns :: SQL.Snippet
    returnedColumns
      | null returnings = "*"
      | otherwise       = SQL.sql $ BS.intercalate ", " (pgFmtColumn (QualifiedIdentifier mempty $ qiName qi) <$> returnings)


-- | SQL query meant for COUNTing the root node of the Tree.
-- It only takes WHERE into account and doesn't include LIMIT/OFFSET because it would reduce the COUNT.
-- SELECT 1 is done instead of SELECT * to prevent doing expensive operations(like functions based on the columns)
-- inside the FROM target.
-- If the request contains INNER JOINs, then the COUNT of the root node will change.
-- For this case, we use a WHERE EXISTS instead of an INNER JOIN on the count query.
-- See https://github.com/PostgREST/postgrest/issues/2009#issuecomment-977473031
-- Only for the nodes that have an INNER JOIN linked to the root level.
readRequestToCountQuery :: ReadRequest -> SQL.Snippet
readRequestToCountQuery (Node (Select{from=mainQi, fromAlias=tblAlias, implicitJoins=implJoins, where_=logicForest, joinConditions=joinConditions_}, _) forest) =
  "SELECT 1 FROM " <> SQL.sql (BS.intercalate ", " (tabl : implJs)) <>
  (if null logicForest && null joinConditions_ && null subQueries
    then mempty
    else " WHERE " ) <>
  intercalateSnippet " AND " (
    map (pgFmtLogicTree treeQi) logicForest ++
    map pgFmtJoinCondition joinConditions_ ++
    subQueries
  )
  where
    treeQi = maybe mainQi (QualifiedIdentifier (qiSchema mainQi)) tblAlias
    tabl = fromQi mainQi <> maybe mempty (\a -> " AS " <> pgFmtIdent a) tblAlias
    implJs = fromQi <$> implJoins
    subQueries = foldr existsSubquery [] forest
    existsSubquery :: ReadRequest -> [SQL.Snippet] -> [SQL.Snippet]
    existsSubquery readReq@(Node (_, (_, _, _, _, joinType, _)) _) rest =
      if joinType == Just JTInner
        then ("EXISTS (" <> readRequestToCountQuery readReq <> " )"):rest
        else rest

limitedQuery :: SQL.Snippet -> Maybe Integer -> SQL.Snippet
limitedQuery query maxRows = query <> SQL.sql (maybe mempty (\x -> " LIMIT " <> BS.pack (show x)) maxRows)
