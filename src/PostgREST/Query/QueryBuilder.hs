{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
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

import qualified Data.Aeson                      as JSON
import qualified Data.ByteString.Char8           as BS
import qualified Data.HashMap.Strict             as HM
import qualified Hasql.DynamicStatements.Snippet as SQL
import qualified Hasql.Encoders                  as HE

import Data.Maybe (fromJust)
import Data.Tree  (Tree (..))

import PostgREST.ApiRequest.Preferences   (PreferResolution (..))
import PostgREST.Config.PgVersion         (PgVersion, pgVersion130)
import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Junction (..),
                                           Relationship (..))
import PostgREST.SchemaCache.Routine      (RoutineParam (..))

import PostgREST.ApiRequest.Types
import PostgREST.Plan.CallPlan
import PostgREST.Plan.MutatePlan
import PostgREST.Plan.ReadPlan
import PostgREST.Plan.Types
import PostgREST.Query.SqlFragment

import Protolude

readPlanToQuery :: ReadPlanTree -> TrackedSnippet
readPlanToQuery node@(Node ReadPlan{select, from = mainQi, fromAlias, where_ = logicForest, order, range_ = readRange, relToParent, relJoinConds, relSelect, relSpread} forest) =
  rawSQL "SELECT " <>
  intercalateSnippet ", " (selects ++ sprExtraSelects ++ joinsSelects) <>
  fromFrag <>
  intercalateSnippet " " joins <>
  (if null logicForest && null relJoinConds
    then mempty
    else rawSQL " WHERE " <> intercalateSnippet " AND " (map (pgFmtLogicTree qi) logicForest ++ map pgFmtJoinCondition relJoinConds)) <> rawSQL " " <>
  groupF qi select relSelect <>
  orderF qi order <>
  limitOffsetF readRange
  where
    fromFrag = fromF relToParent mainQi fromAlias
    qi = getQualifiedIdentifier relToParent mainQi fromAlias
    -- gets all the columns in case of an empty select, ignoring/obtaining these columns is done at the aggregation stage
    defSelect = [CoercibleSelectField (unknownField "*" []) Nothing Nothing Nothing Nothing]
    joins = getJoins node
    selects = pgFmtSelectItem qi <$> (if null select && null forest then defSelect else select)
    joinsSelects = getJoinSelects node
    sprExtraSelects = case relSpread of
      Just (ToManySpread sels _) -> (\s -> pgFmtSelectItem (maybe qi (QualifiedIdentifier "") $ fst s) $ snd s) <$> sels
      _ -> mempty

getJoinSelects :: ReadPlanTree -> [TrackedSnippet]
getJoinSelects (Node ReadPlan{relSelect} _) =
  mapMaybe relSelectToSnippet relSelect
  where
    relSelectToSnippet :: RelSelectField -> Maybe TrackedSnippet
    relSelectToSnippet fld =
      let aggAlias = pgFmtIdent $ rsAggAlias fld
      in
        case fld of
          JsonEmbed{rsEmptyEmbed = True} ->
            Nothing
          JsonEmbed{rsSelName, rsEmbedMode = JsonObject} ->
            Just $ rawSQL "row_to_json(" <> aggAlias <> rawSQL ".*)::jsonb AS " <> pgFmtIdent rsSelName
          JsonEmbed{rsSelName, rsEmbedMode = JsonArray} ->
            Just $ rawSQL "COALESCE( " <> aggAlias <> rawSQL "." <> aggAlias <> rawSQL ", '[]') AS " <> pgFmtIdent rsSelName
          Spread{rsSpreadSel, rsAggAlias} ->
            Just $ intercalateSnippet ", " (pgFmtSpreadSelectItem rsAggAlias <$> rsSpreadSel)

getJoins :: ReadPlanTree -> [TrackedSnippet]
getJoins (Node _ []) = []
getJoins (Node ReadPlan{relSelect} forest) =
  map (\fld ->
         let alias = rsAggAlias fld
             matchingNode = fromJust $ find (\(Node ReadPlan{relAggAlias} _) -> alias == relAggAlias) forest
         in getJoin fld matchingNode
      ) relSelect

getJoin :: RelSelectField -> ReadPlanTree -> TrackedSnippet
getJoin fld node@(Node ReadPlan{relJoinType, relSpread} _) =
  let
    correlatedSubquery sub al cond =
      rawSQL " " <> (if relJoinType == Just JTInner then rawSQL "INNER" else rawSQL "LEFT") <> rawSQL " JOIN LATERAL ( " <> sub <> rawSQL " ) AS " <> al <> rawSQL " ON " <> cond
    subquery = readPlanToQuery node
    aggAlias = pgFmtIdent $ rsAggAlias fld
    selectSubqAgg = rawSQL "SELECT json_agg(" <> aggAlias <> rawSQL ")::jsonb AS " <> aggAlias
    fromSubqAgg = rawSQL " FROM (" <> subquery <> rawSQL " ) AS " <> aggAlias
    joinCondition = if relJoinType == Just JTInner then aggAlias <> rawSQL " IS NOT NULL" else rawSQL "TRUE"
  in
    case fld of
      JsonEmbed{rsEmbedMode = JsonObject} ->
        correlatedSubquery subquery aggAlias (rawSQL "TRUE")
      Spread{rsSpreadSel, rsAggAlias} ->
        case relSpread of
          Just (ToManySpread _ sprOrder) ->
            let selSpread = selectSubqAgg <> (if null rsSpreadSel then mempty else rawSQL ", ") <> intercalateSnippet ", " (pgFmtSpreadJoinSelectItem rsAggAlias sprOrder <$> rsSpreadSel)
            in correlatedSubquery (selSpread <> fromSubqAgg) aggAlias joinCondition
          _ ->
            correlatedSubquery subquery aggAlias (rawSQL "TRUE")
      JsonEmbed{rsEmbedMode = JsonArray} ->
        correlatedSubquery (selectSubqAgg <> fromSubqAgg) aggAlias joinCondition

mutatePlanToQuery :: MutatePlan -> TrackedSnippet
mutatePlanToQuery (Insert mainQi iCols body onConflict putConditions returnings _ applyDefaults) =
  rawSQL "INSERT INTO " <> fromQi mainQi <> (if null iCols then rawSQL " " else rawSQL "(" <> cols <> rawSQL ") ") <>
  fromJsonBodyF body iCols True False applyDefaults <>
  -- Only used for PUT
  (if null putConditions then mempty else rawSQL "WHERE " <> addConfigPgrstInserted True <> rawSQL " AND " <> intercalateSnippet " AND " (pgFmtLogicTree (QualifiedIdentifier mempty "pgrst_body") <$> putConditions)) <>
  (if null putConditions && mergeDups then rawSQL "WHERE " <> addConfigPgrstInserted True else mempty) <> 
  maybe mempty ( \(oncDo, oncCols) ->
    if null oncCols then
      mempty
    else
      rawSQL " ON CONFLICT(" <> intercalateSnippet ", " (pgFmtIdent <$> oncCols) <> rawSQL ") " <> case oncDo of
      IgnoreDuplicates ->
        rawSQL "DO NOTHING"
      MergeDuplicates  ->
        if null iCols
            then rawSQL "DO NOTHING"
            else rawSQL "DO UPDATE SET " <> intercalateSnippet ", " ((pgFmtIdent . cfName) <> const (rawSQL " = EXCLUDED.") <> (pgFmtIdent . cfName) <$> iCols) <> (if null putConditions && not mergeDups then mempty else rawSQL "WHERE " <> addConfigPgrstInserted False)
    ) onConflict <> rawSQL " " <>
    returningF mainQi returnings
  where
    cols = intercalateSnippet ", " $ pgFmtIdent . cfName <$> iCols
    mergeDups = case onConflict of Just (MergeDuplicates, _) -> True; _ -> False

mutatePlanToQuery (Update mainQi uCols body logicForest returnings applyDefaults)
  | null uCols =
    -- if there are no columns we cannot do UPDATE table SET {empty}, it'd be invalid syntax
    -- selecting an empty resultset from mainQi gives us the column names to prevent errors when using &select=
    -- the select has to be based on "returnings" to make computed overloaded functions not throw
    rawSQL "SELECT " <> emptyBodyReturnedColumns <> rawSQL " FROM " <> fromQi mainQi <> rawSQL " WHERE false"

  | otherwise =
    rawSQL "UPDATE " <> mainTbl <> rawSQL " SET " <> cols <> rawSQL " " <>
    fromJsonBodyF body uCols False False applyDefaults <>
    whereLogic <> rawSQL " " <>
    returningF mainQi returnings

  where
    whereLogic = if null logicForest then mempty else rawSQL " WHERE " <> intercalateSnippet " AND " (pgFmtLogicTree mainQi <$> logicForest)
    mainTbl = fromQi mainQi
    emptyBodyReturnedColumns = if null returnings then rawSQL "NULL" else intercalateSnippet ", " (pgFmtColumn (QualifiedIdentifier mempty $ qiName mainQi) <$> returnings)
    cols = intercalateSnippet ", " (pgFmtIdent . cfName <> const (rawSQL " = ") <> pgFmtColumn (QualifiedIdentifier mempty "pgrst_body") . cfName <$> uCols)

mutatePlanToQuery (Delete mainQi logicForest returnings) =
  rawSQL "DELETE FROM " <> fromQi mainQi <> rawSQL " " <>
  whereLogic <> rawSQL " " <>
  returningF mainQi returnings
  where
    whereLogic = if null logicForest then mempty else rawSQL " WHERE " <> intercalateSnippet " AND " (pgFmtLogicTree mainQi <$> logicForest)

callPlanToQuery :: CallPlan -> PgVersion -> TrackedSnippet
callPlanToQuery (FunctionCall qi params arguments returnsScalar returnsSetOfScalar returnsCompositeAlias returnings) pgVer =
  rawSQL "SELECT " <> (if returnsScalar || returnsSetOfScalar then rawSQL "pgrst_call.pgrst_scalar" else returnedColumns) <> rawSQL " " <>
  fromCall
  where
    jsonArgs = case arguments of
      DirectArgs args -> Just $ JSON.encode args
      JsonArgs json   -> json
    fromCall = case params of
      OnePosParam prm -> rawSQL "FROM " <> callIt (singleParameter jsonArgs $ encodeUtf8 $ ppType prm)
      KeyParams []    -> rawSQL "FROM " <> callIt mempty
      KeyParams prms  -> case arguments of
        DirectArgs args -> rawSQL "FROM " <> callIt (fmtArgs prms args)
        JsonArgs json   -> fromJsonBodyF json ((\p -> CoercibleField (ppName p) mempty False Nothing (ppTypeMaxLength p) Nothing Nothing False) <$> prms) False True False <> rawSQL ", " <> 
                        rawSQL "LATERAL " <> callIt (fmtParams prms)

    callIt :: TrackedSnippet -> TrackedSnippet
    callIt argument | pgVer < pgVersion130 && returnsCompositeAlias = rawSQL "(SELECT (" <> fromQi qi <> rawSQL "(" <> argument <> rawSQL ")).*) pgrst_call"
                    | returnsScalar || returnsSetOfScalar           = rawSQL "(SELECT " <> fromQi qi <> rawSQL "(" <> argument <> rawSQL ") pgrst_scalar) pgrst_call"
                    | otherwise                                     = fromQi qi <> rawSQL "(" <> argument <> rawSQL ") pgrst_call"

    fmtParams :: [RoutineParam] -> TrackedSnippet
    fmtParams prms = intercalateSnippet ", "
      ((\a -> (if ppVar a then rawSQL "VARIADIC " else mempty) <> pgFmtIdent (ppName a) <> rawSQL " := pgrst_body." <> pgFmtIdent (ppName a)) <$> prms)

    fmtArgs :: [RoutineParam] -> HM.HashMap Text RpcParamValue -> TrackedSnippet
    fmtArgs prms args = intercalateSnippet ", " $ fmtArg <$> prms
      where
        fmtArg RoutineParam{..} =
          (if ppVar then rawSQL "VARIADIC " else mempty) <> 
          pgFmtIdent ppName <> 
          rawSQL " := " <>
          encodeArg (HM.lookup ppName args) <>
          rawSQL "::" <> 
          rawSQL (encodeUtf8 ppTypeMaxLength)
        encodeArg :: Maybe RpcParamValue -> TrackedSnippet
        encodeArg (Just (Variadic v)) = fromSnippet (SQL.encoderAndParam (HE.nonNullable $ HE.foldableArray $ HE.nonNullable HE.text) v)
        encodeArg (Just (Fixed v)) = fromSnippet (SQL.encoderAndParam (HE.nonNullable HE.unknown) $ encodeUtf8 v)
        -- Currently not supported: Calling functions without some of their arguments without DEFAULT.
        -- We could fallback to providing this NULL value in those cases.
        encodeArg Nothing = rawSQL "NULL"

    returnedColumns :: TrackedSnippet
    returnedColumns
      | null returnings = rawSQL "*"
      | otherwise       = intercalateSnippet ", " (pgFmtColumn (QualifiedIdentifier mempty "pgrst_call") <$> returnings)

-- | SQL query meant for COUNTing the root node of the Tree.
-- It only takes WHERE into account and doesn't include LIMIT/OFFSET because it would reduce the COUNT.
-- SELECT 1 is done instead of SELECT * to prevent doing expensive operations(like functions based on the columns)
-- inside the FROM target.
-- If the request contains INNER JOINs, then the COUNT of the root node will change.
-- For this case, we use a WHERE EXISTS instead of an INNER JOIN on the count query.
-- See https://github.com/PostgREST/postgrest/issues/2009#issuecomment-977473031
-- Only for the nodes that have an INNER JOIN linked to the root level.
readPlanToCountQuery :: ReadPlanTree -> TrackedSnippet
readPlanToCountQuery (Node ReadPlan{from = mainQi, fromAlias = tblAlias, where_ = logicForest, relToParent = rel, relJoinConds} forest) =
  rawSQL "SELECT 1 " <> fromFrag <>
  (if null logicForest && null relJoinConds && null subQueries
    then mempty
    else rawSQL " WHERE " ) <>
  intercalateSnippet " AND " (
    map (pgFmtLogicTreeCount qi) logicForest ++
    map pgFmtJoinCondition relJoinConds ++
    subQueries
  )
  where
    qi = getQualifiedIdentifier rel mainQi tblAlias
    fromFrag = fromF rel mainQi tblAlias
    subQueries = foldr existsSubquery [] forest
    existsSubquery :: ReadPlanTree -> [TrackedSnippet] -> [TrackedSnippet]
    existsSubquery readReq@(Node ReadPlan{relJoinType = joinType} _) rest =
      if joinType == Just JTInner
        then (rawSQL "EXISTS (" <> readPlanToCountQuery readReq <> rawSQL " )") : rest
        else rest
    findNullEmbedRel fld = find (\(Node ReadPlan{relAggAlias} _) -> fld == relAggAlias) forest

    -- https://github.com/PostgREST/postgrest/pull/2930#discussion_r1325293698
    pgFmtLogicTreeCount :: QualifiedIdentifier -> CoercibleLogicTree -> TrackedSnippet
    pgFmtLogicTreeCount qiCount (CoercibleExpr hasNot op frst) = rawSQL notOp <> rawSQL " (" <> intercalateSnippet (opSql op) (pgFmtLogicTreeCount qiCount <$> frst) <> rawSQL ")"
      where
        notOp = if hasNot then "NOT" else mempty
        opSql And = " AND "
        opSql Or  = " OR "
    pgFmtLogicTreeCount _ (CoercibleStmnt (CoercibleFilterNullEmbed hasNot fld)) =
      maybe mempty (\x -> (if not hasNot then rawSQL "NOT " else mempty) <> rawSQL "EXISTS (" <> readPlanToCountQuery x <> rawSQL ")") (findNullEmbedRel fld)
    pgFmtLogicTreeCount qiCount (CoercibleStmnt flt) = pgFmtFilter qiCount flt

limitedQuery :: TrackedSnippet -> Maybe Integer -> TrackedSnippet
limitedQuery query maxRows = query <> rawSQL (maybe mempty (\x -> " LIMIT " <> BS.pack (show x)) maxRows)

-- TODO refactor so this function is uneeded and ComputedRelationship QualifiedIdentifier comes from the ReadPlan type
getQualifiedIdentifier :: Maybe Relationship -> QualifiedIdentifier -> Maybe Alias -> QualifiedIdentifier
getQualifiedIdentifier rel mainQi tblAlias = case rel of
  Just ComputedRelationship{relFunction} -> QualifiedIdentifier mempty $ fromMaybe (qiName relFunction) tblAlias
  _                                      -> maybe mainQi (QualifiedIdentifier mempty) tblAlias

-- FROM clause plus implicit joins
fromF :: Maybe Relationship -> QualifiedIdentifier -> Maybe Alias -> TrackedSnippet
fromF rel mainQi tblAlias = rawSQL " FROM " <>
  (case rel of
    -- Due to the use of CTEs on RPC, we need to cast the parameter to the table name in case of function overloading.
    -- See https://github.com/PostgREST/postgrest/issues/2963#issuecomment-1736557386
    Just ComputedRelationship{relFunction,relTableAlias,relTable} -> fromQi relFunction <> rawSQL "(" <> pgFmtIdent (qiName relTableAlias) <> rawSQL "::" <> fromQi relTable <> rawSQL ")"
    _                                                             -> fromQi mainQi) <>
  maybe mempty (\a -> rawSQL " AS " <> pgFmtIdent a) tblAlias <>
  (case rel of
    Just Relationship{relCardinality=M2M Junction{junTable=jt}} -> rawSQL ", " <> fromQi jt
    _                                                           -> mempty)
