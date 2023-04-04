{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-|
Module      : PostgREST.Query.QueryBuilder
Description : PostgREST SQL queries generating functions.

This module provides functions to consume data types that
represent database queries (e.g. ReadPlanTree, MutatePlan) and SqlFragment
to produce SqlQuery type outputs.
-}
module PostgREST.Query.QueryBuilder
  ( readPlanToQuery
  , mutatePlanToQuery
  , readPlanToCountQuery
  , callPlanToQuery
  , limitedQuery
  ) where

import qualified Data.ByteString.Char8           as BS
import qualified Hasql.DynamicStatements.Snippet as SQL

import Data.Tree (Tree (..))

import PostgREST.ApiRequest.Preferences   (PreferResolution (..))
import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Proc         (ProcParam (..))
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..),
                                           relIsToOne)

import PostgREST.ApiRequest.Types
import PostgREST.Plan.CallPlan
import PostgREST.Plan.MutatePlan
import PostgREST.Plan.ReadPlan
import PostgREST.Plan.Types
import PostgREST.Query.SqlFragment
import PostgREST.RangeQuery        (allRange)

import Protolude

readPlanToQuery :: ReadPlanTree -> SQL.Snippet
readPlanToQuery (Node ReadPlan{select,from=mainQi,fromAlias,where_=logicForest,order, range_=readRange, relToParent, relJoinConds} forest) =
  "SELECT " <>
  intercalateSnippet ", " ((pgFmtSelectItem qi <$> (if null select && null forest then defSelect else select)) ++ selects) <> " " <>
  fromFrag <> " " <>
  intercalateSnippet " " joins <> " " <>
  (if null logicForest && null relJoinConds
    then mempty
    else "WHERE " <> intercalateSnippet " AND " (map (pgFmtLogicTree qi) logicForest ++ map pgFmtJoinCondition relJoinConds)) <> " " <>
  orderF qi order <> " " <>
  limitOffsetF readRange
  where
    fromFrag = fromF relToParent mainQi fromAlias
    qi = getQualifiedIdentifier relToParent mainQi fromAlias
    defSelect = [(("*", []), Nothing, Nothing)] -- gets all the columns in case of an empty select, ignoring/obtaining these columns is done at the aggregation stage
    (selects, joins) = foldr getSelectsJoins ([],[]) forest

getSelectsJoins :: ReadPlanTree -> ([SQL.Snippet], [SQL.Snippet]) -> ([SQL.Snippet], [SQL.Snippet])
getSelectsJoins (Node ReadPlan{relToParent=Nothing} _) _ = ([], [])
getSelectsJoins rr@(Node ReadPlan{select, relName, relToParent=Just rel, relAggAlias, relAlias, relJoinType, relIsSpread} forest) (selects,joins) =
  let
    subquery = readPlanToQuery rr
    aliasOrName = pgFmtIdent $ fromMaybe relName relAlias
    aggAlias = pgFmtIdent relAggAlias
    correlatedSubquery sub al cond =
      (if relJoinType == Just JTInner then "INNER" else "LEFT") <> " JOIN LATERAL ( " <> sub <> " ) AS " <> SQL.sql al <> " ON " <> cond
    (sel, joi) = if relIsToOne rel
      then
        ( if relIsSpread
            then SQL.sql aggAlias <> ".*"
            else SQL.sql ("row_to_json(" <> aggAlias <> ".*) AS " <> aliasOrName)
        , correlatedSubquery subquery aggAlias "TRUE")
      else
        ( SQL.sql $ "COALESCE( " <> aggAlias <> "." <> aggAlias <> ", '[]') AS " <> aliasOrName
        , correlatedSubquery (
            "SELECT json_agg(" <> SQL.sql aggAlias <> ") AS " <> SQL.sql aggAlias <>
            "FROM (" <> subquery <> " ) AS " <> SQL.sql aggAlias
          ) aggAlias $ if relJoinType == Just JTInner then SQL.sql aggAlias <> " IS NOT NULL" else "TRUE")
  in
  (if null select && null forest then selects else sel:selects, joi:joins)

mutatePlanToQuery :: MutatePlan -> SQL.Snippet
mutatePlanToQuery (Insert mainQi iCols body onConflct putConditions returnings _ applyDefaults) =
  "INSERT INTO " <> SQL.sql (fromQi mainQi) <> SQL.sql (if null iCols then " " else "(" <> cols <> ") ") <>
  fromJsonBodyF body iCols True False False applyDefaults <>
  -- Only used for PUT
  (if null putConditions then mempty else "WHERE " <> intercalateSnippet " AND " (pgFmtLogicTree (QualifiedIdentifier mempty "pgrst_body") <$> putConditions)) <>
  SQL.sql (BS.unwords [
    maybe mempty (\(oncDo, oncCols) ->
      if null oncCols then
        mempty
      else
        " ON CONFLICT(" <> BS.intercalate ", " (pgFmtIdent <$> oncCols) <> ") " <> case oncDo of
        IgnoreDuplicates ->
          "DO NOTHING"
        MergeDuplicates  ->
          if null iCols
             then "DO NOTHING"
             else "DO UPDATE SET " <> BS.intercalate ", " ((pgFmtIdent . tfName) <> const " = EXCLUDED." <> (pgFmtIdent . tfName) <$> iCols)
      ) onConflct,
    returningF mainQi returnings
    ])
  where
    cols = BS.intercalate ", " $ pgFmtIdent . tfName <$> iCols

-- An update without a limit is always filtered with a WHERE
mutatePlanToQuery (Update mainQi uCols body logicForest range ordts returnings applyDefaults)
  | null uCols =
    -- if there are no columns we cannot do UPDATE table SET {empty}, it'd be invalid syntax
    -- selecting an empty resultset from mainQi gives us the column names to prevent errors when using &select=
    -- the select has to be based on "returnings" to make computed overloaded functions not throw
    SQL.sql $ "SELECT " <> emptyBodyReturnedColumns <> " FROM " <> fromQi mainQi <> " WHERE false"

  | range == allRange =
    "UPDATE " <> mainTbl <> " SET " <> SQL.sql nonRangeCols <> " " <>
    fromJsonBodyF body uCols False False False applyDefaults <>
    whereLogic <> " " <>
    SQL.sql (returningF mainQi returnings)

  | otherwise =
    "WITH " <>
    "pgrst_update_body AS (" <> fromJsonBodyF body uCols True False True applyDefaults <> "), " <>
    "pgrst_affected_rows AS (" <>
      "SELECT " <> SQL.sql rangeIdF <> " FROM " <> mainTbl <>
      whereLogic <> " " <>
      orderF mainQi ordts <> " " <>
      limitOffsetF range <>
    ") " <>
    "UPDATE " <> mainTbl <> " SET " <> SQL.sql rangeCols <>
    "FROM pgrst_affected_rows " <>
    "WHERE " <> SQL.sql whereRangeIdF <> " " <>
    SQL.sql (returningF mainQi returnings)

  where
    whereLogic = if null logicForest then mempty else " WHERE " <> intercalateSnippet " AND " (pgFmtLogicTree mainQi <$> logicForest)
    mainTbl = SQL.sql (fromQi mainQi)
    emptyBodyReturnedColumns = if null returnings then "NULL" else BS.intercalate ", " (pgFmtColumn (QualifiedIdentifier mempty $ qiName mainQi) <$> returnings)
    nonRangeCols = BS.intercalate ", " (pgFmtIdent . tfName <> const " = " <> pgFmtColumn (QualifiedIdentifier mempty "pgrst_body") . tfName <$> uCols)
    rangeCols = BS.intercalate ", " ((\col -> pgFmtIdent (tfName col) <> " = (SELECT " <> pgFmtIdent (tfName col) <> " FROM pgrst_update_body) ") <$> uCols)
    (whereRangeIdF, rangeIdF) = mutRangeF mainQi (fst . otTerm <$> ordts)

mutatePlanToQuery (Delete mainQi dCols body logicForest range ordts returnings pkFlts isBulk)
  | range == allRange =
    "DELETE FROM " <> SQL.sql (fromQi mainQi) <> " " <>
    (if isBulk
      then fromJsonBodyF body dCols False True False False <> whereLogicBulk
      else whereLogic) <> " " <>
    SQL.sql (returningF mainQi returnings)

  | otherwise =
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
    whereLogic = pgFmtWhereF (null logicForest) logicForestF
    whereLogicBulk = pgFmtWhereF (null logicForest && null pkFlts) (logicForestF <> pgrstDeleteBodyF)
    pgFmtWhereF hasEmptyLogic flts = if hasEmptyLogic then mempty else " WHERE " <> intercalateSnippet " AND " flts
    logicForestF = pgFmtLogicTree mainQi <$> logicForest
    pgrstDeleteBodyF = pgFmtBodyFilter mainQi (QualifiedIdentifier mempty "pgrst_body") <$> pkFlts
    pgFmtBodyFilter table cte f = SQL.sql (pgFmtColumn table f <> " = " <> pgFmtColumn cte f)
    (whereRangeIdF, rangeIdF) = mutRangeF mainQi (fst . otTerm <$> ordts)

callPlanToQuery :: CallPlan -> SQL.Snippet
callPlanToQuery (FunctionCall qi params args returnsScalar multipleCall returnings) =
  "SELECT " <> (if returnsScalar then "pgrst_call AS pgrst_scalar " else returnedColumns) <> " " <>
  fromCall
  where
    fromCall = case params of
      OnePosParam prm -> "FROM " <> callIt (singleParameter args $ encodeUtf8 $ ppType prm)
      KeyParams []    -> "FROM " <> callIt mempty
      KeyParams prms  -> fromJsonBodyF args ((\p -> TypedField (ppName p) (ppType p) Nothing) <$> prms) False False (not multipleCall) False <> ", " <>
                         "LATERAL " <> callIt (fmtParams prms)

    callIt :: SQL.Snippet -> SQL.Snippet
    callIt argument = SQL.sql (fromQi qi) <> "(" <> argument <> ") pgrst_call"

    fmtParams :: [ProcParam] -> SQL.Snippet
    fmtParams prms = SQL.sql $ BS.intercalate ", "
      ((\a -> (if ppVar a then "VARIADIC " else mempty) <> pgFmtIdent (ppName a) <> " := pgrst_body." <> pgFmtIdent (ppName a)) <$> prms)

    returnedColumns :: SQL.Snippet
    returnedColumns
      | null returnings = "*"
      | otherwise       = SQL.sql $ BS.intercalate ", " (pgFmtColumn (QualifiedIdentifier mempty "pgrst_call") <$> returnings)

-- | SQL query meant for COUNTing the root node of the Tree.
-- It only takes WHERE into account and doesn't include LIMIT/OFFSET because it would reduce the COUNT.
-- SELECT 1 is done instead of SELECT * to prevent doing expensive operations(like functions based on the columns)
-- inside the FROM target.
-- If the request contains INNER JOINs, then the COUNT of the root node will change.
-- For this case, we use a WHERE EXISTS instead of an INNER JOIN on the count query.
-- See https://github.com/PostgREST/postgrest/issues/2009#issuecomment-977473031
-- Only for the nodes that have an INNER JOIN linked to the root level.
readPlanToCountQuery :: ReadPlanTree -> SQL.Snippet
readPlanToCountQuery (Node ReadPlan{from=mainQi, fromAlias=tblAlias, where_=logicForest, relToParent=rel, relJoinConds} forest) =
  "SELECT 1 " <> fromFrag <>
  (if null logicForest && null relJoinConds && null subQueries
    then mempty
    else " WHERE " ) <>
  intercalateSnippet " AND " (
    map (pgFmtLogicTree qi) logicForest ++
    map pgFmtJoinCondition relJoinConds ++
    subQueries
  )
  where
    qi = getQualifiedIdentifier rel mainQi tblAlias
    fromFrag = fromF rel mainQi tblAlias
    subQueries = foldr existsSubquery [] forest
    existsSubquery :: ReadPlanTree -> [SQL.Snippet] -> [SQL.Snippet]
    existsSubquery readReq@(Node ReadPlan{relJoinType=joinType} _) rest =
      if joinType == Just JTInner
        then ("EXISTS (" <> readPlanToCountQuery readReq <> " )"):rest
        else rest

limitedQuery :: SQL.Snippet -> Maybe Integer -> SQL.Snippet
limitedQuery query maxRows = query <> SQL.sql (maybe mempty (\x -> " LIMIT " <> BS.pack (show x)) maxRows)

-- TODO refactor so this function is uneeded and ComputedRelationship QualifiedIdentifier comes from the ReadPlan type
getQualifiedIdentifier :: Maybe Relationship -> QualifiedIdentifier -> Maybe Alias -> QualifiedIdentifier
getQualifiedIdentifier rel mainQi tblAlias = case rel of
  Just ComputedRelationship{relFunction} -> QualifiedIdentifier mempty $ fromMaybe (qiName relFunction) tblAlias
  _                                      -> maybe mainQi (QualifiedIdentifier mempty) tblAlias

-- FROM clause plus implicit joins
fromF :: Maybe Relationship -> QualifiedIdentifier -> Maybe Alias -> SQL.Snippet
fromF rel mainQi tblAlias = SQL.sql $ "FROM " <>
  (case rel of
    Just ComputedRelationship{relFunction,relTable} -> fromQi relFunction <> "(" <> pgFmtIdent (qiName relTable) <> ")"
    _                                               -> fromQi mainQi) <>
  maybe mempty (\a -> " AS " <> pgFmtIdent a) tblAlias <>
  (case rel of
    Just Relationship{relCardinality=M2M Junction{junTable=jt}} -> ", " <> fromQi jt
    _                                                           -> mempty)
