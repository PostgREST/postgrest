{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-|
Module      : PostgREST.QueryBuilder
Description : PostgREST SQL queries generating functions.

This module provides functions to consume data types that
represent database queries (e.g. ReadRequest, MutateRequest) and SqlFragment
to produce SqlQuery type outputs.
-}
module PostgREST.QueryBuilder (
    readRequestToQuery
  , mutateRequestToQuery
  , readRequestToCountQuery
  , requestToCallProcQuery
  , limitedQuery
  , setLocalQuery
  , setLocalSearchPathQuery
  ) where

import qualified Data.Set as S

import Data.Text (intercalate, unwords)
import Data.Tree (Tree (..))

import Data.Maybe

import PostgREST.Private.QueryFragment
import PostgREST.RangeQuery            (allRange, rangeLimit,
                                        rangeOffset)
import PostgREST.Types
import Protolude                       hiding (cast, intercalate,
                                        replace)

readRequestToQuery :: Bool -> ReadRequest -> SqlQuery
readRequestToQuery isParent (Node (Select colSelects mainQi tblAlias implJoins logicForest joinConditions_ ordts range, _) forest) =
  unwords [
    "SELECT " <> intercalate ", " (map (pgFmtSelectItem qi) colSelects ++ selects),
    "FROM " <> intercalate ", " (tabl : implJs),
    unwords joins,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest ++ map pgFmtJoinCondition joinConditions_))
      `emptyOnFalse` (null logicForest && null joinConditions_),
    ("ORDER BY " <> intercalate ", " (map (pgFmtOrderTerm qi) ordts)) `emptyOnFalse` null ordts,
    ("LIMIT " <> maybe "ALL" show (rangeLimit range) <> " OFFSET " <> show (rangeOffset range)) `emptyOnFalse` (isParent || range == allRange) ]

  where
    implJs = fromQi <$> implJoins
    tabl = fromQi mainQi <> maybe mempty (\a -> " AS " <> pgFmtIdent a) tblAlias
    qi = maybe mainQi (QualifiedIdentifier mempty) tblAlias

    (joins, selects) = foldr getQueryParts ([],[]) forest

    getQueryParts :: Tree ReadNode -> ([SqlFragment], [SqlFragment]) -> ([SqlFragment], [SqlFragment])
    getQueryParts (Node n@(_, (name, Just Relation{relType=Child,relTable=Table{tableName=table}}, alias, _, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE(("
           <> "SELECT json_agg(" <> pgFmtIdent table <> ".*) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = readRequestToQuery False (Node n forst)
    getQueryParts (Node n@(_, (name, Just Relation{relType=Parent,relTable=Table{tableName=table}}, alias, _, _)) forst) (j,s) = (joi:j,sel:s)
      where
        aliasOrName = fromMaybe name alias
        localTableName = pgFmtIdent $ table <> "_" <> aliasOrName
        sel = "row_to_json(" <> localTableName <> ".*) AS " <> pgFmtIdent aliasOrName
        joi = " LEFT JOIN LATERAL( " <> subquery <> " ) AS " <> localTableName <> " ON TRUE "
          where subquery = readRequestToQuery True (Node n forst)
    getQueryParts (Node n@(_, (name, Just Relation{relType=Many,relTable=Table{tableName=table}}, alias, _, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE (("
           <> "SELECT json_agg(" <> pgFmtIdent table <> ".*) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = readRequestToQuery False (Node n forst)
    --the following is just to remove the warning
    --getQueryParts is not total but readRequestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts _ _ = witness


mutateRequestToQuery :: MutateRequest -> SqlQuery
mutateRequestToQuery (Insert mainQi iCols onConflct putConditions returnings) =
  unwords [
    "WITH " <> normalizedBody,
    "INSERT INTO ", fromQi mainQi, if S.null iCols then " " else "(" <> cols <> ")",
    unwords [
      "SELECT " <> cols <> " FROM",
      "json_populate_recordset", "(null::", fromQi mainQi, ", " <> selectBody <> ") _",
      -- Only used for PUT
      ("WHERE " <> intercalate " AND " (pgFmtLogicTree (QualifiedIdentifier mempty "_") <$> putConditions)) `emptyOnFalse` null putConditions],
    maybe "" (\(oncDo, oncCols) -> (
      "ON CONFLICT(" <> intercalate ", " (pgFmtIdent <$> oncCols) <> ") " <> case oncDo of
      IgnoreDuplicates ->
        "DO NOTHING"
      MergeDuplicates  ->
        if S.null iCols
           then "DO NOTHING"
           else "DO UPDATE SET " <> intercalate ", " (pgFmtIdent <> const " = EXCLUDED." <> pgFmtIdent <$> S.toList iCols)
                                   ) `emptyOnFalse` null oncCols) onConflct,
    ("RETURNING " <> intercalate ", " (map (pgFmtColumn mainQi) returnings)) `emptyOnFalse` null returnings]
  where
    cols = intercalate ", " $ pgFmtIdent <$> S.toList iCols
mutateRequestToQuery (Update mainQi uCols logicForest returnings) =
  if S.null uCols
    then "WITH " <> ignoredBody <> "SELECT null WHERE false" -- if there are no columns we cannot do UPDATE table SET {empty}, it'd be invalid syntax
    else
      unwords [
        "WITH " <> normalizedBody,
        "UPDATE " <> fromQi mainQi <> " SET " <> cols,
        "FROM (SELECT * FROM json_populate_recordset", "(null::", fromQi mainQi, ", " <> selectBody <> ")) _ ",
        ("WHERE " <> intercalate " AND " (pgFmtLogicTree mainQi <$> logicForest)) `emptyOnFalse` null logicForest,
        ("RETURNING " <> intercalate ", " (pgFmtColumn mainQi <$> returnings)) `emptyOnFalse` null returnings
        ]
  where
    cols = intercalate ", " (pgFmtIdent <> const " = _." <> pgFmtIdent <$> S.toList uCols)
mutateRequestToQuery (Delete mainQi logicForest returnings) =
  unwords [
    "WITH " <> ignoredBody,
    "DELETE FROM ", fromQi mainQi,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree mainQi) logicForest)) `emptyOnFalse` null logicForest,
    ("RETURNING " <> intercalate ", " (map (pgFmtColumn mainQi) returnings)) `emptyOnFalse` null returnings
    ]

requestToCallProcQuery :: QualifiedIdentifier -> [PgArg] -> Bool -> Maybe PreferParameters -> SqlQuery
requestToCallProcQuery qi pgArgs returnsScalar preferParams =
  unwords [
    "WITH",
    argsCTE,
    sourceBody ]
  where
    paramsAsSingleObject    = preferParams == Just SingleObject
    paramsAsMulitpleObjects = preferParams == Just MultipleObjects

    (argsCTE, args)
      | null pgArgs = (ignoredBody, "")
      | paramsAsSingleObject = ("pgrst_args AS (SELECT NULL)", "$1::json")
      | otherwise = (
          unwords [
            normalizedBody <> ",",
            "pgrst_args AS (",
              "SELECT * FROM json_to_recordset(" <> selectBody <> ") AS _(" <> fmtArgs (\a -> " " <> pgaType a) <> ")",
            ")"]
         , if paramsAsMulitpleObjects
             then fmtArgs (\a -> " := pgrst_args." <> pgFmtIdent (pgaName a))
             else fmtArgs (\a -> " := (SELECT " <> pgFmtIdent (pgaName a) <> " FROM pgrst_args LIMIT 1)")
        )

    fmtArgs :: (PgArg -> SqlFragment) -> SqlFragment
    fmtArgs argFrag = intercalate ", " ((\a -> pgFmtIdent (pgaName a) <> argFrag a) <$> pgArgs)

    sourceBody :: SqlFragment
    sourceBody
      | paramsAsMulitpleObjects =
          if returnsScalar
            then "SELECT " <> callIt <> " AS pgrst_scalar FROM pgrst_args"
            else unwords [ "SELECT pgrst_lat_args.*"
                         , "FROM pgrst_args,"
                         , "LATERAL ( SELECT * FROM " <> callIt <> " ) pgrst_lat_args" ]
      | otherwise =
          if returnsScalar
            then "SELECT " <> callIt <> " AS pgrst_scalar"
            else "SELECT * FROM " <> callIt

    callIt :: SqlFragment
    callIt = fromQi qi <> "(" <> args <> ")"


-- | SQL query meant for COUNTing the root node of the Tree.
-- It only takes WHERE into account and doesn't include LIMIT/OFFSET because it would reduce the COUNT.
-- SELECT 1 is done instead of SELECT * to prevent doing expensive operations(like functions based on the columns)
-- inside the FROM target.
readRequestToCountQuery :: ReadRequest -> SqlQuery
readRequestToCountQuery (Node (Select{from=qi, where_=logicForest}, _) _) =
 unwords [
   "SELECT 1",
   "FROM " <> fromQi qi,
   ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest
   ]

limitedQuery :: SqlQuery -> Maybe Integer -> SqlQuery
limitedQuery query maxRows = query <> maybe mempty (\x -> " LIMIT " <> show x) maxRows

setLocalQuery :: Text -> (Text, Text) -> SqlQuery
setLocalQuery prefix (k, v) =
  "SET LOCAL " <> pgFmtIdent (prefix <> k) <> " = " <> pgFmtLit v <> ";"

setLocalSearchPathQuery :: [Text] -> SqlQuery
setLocalSearchPathQuery vals =
  "SET LOCAL search_path = " <> intercalate ", " (pgFmtLit <$> vals) <> ";"
