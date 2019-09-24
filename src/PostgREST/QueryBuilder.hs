{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-|
Module      : PostgREST.QueryBuilder
Description : PostgREST SQL queries generating functions.

This module provides functions to consume data types that
represent database objects (e.g. Relation, Schema) and SqlFragment
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

import PostgREST.QueryBuilder.Private
import PostgREST.RangeQuery           (allRange, rangeLimit,
                                       rangeOffset)
import PostgREST.Types
import Protolude                      hiding (cast, intercalate,
                                       replace)

readRequestToQuery :: Schema -> Bool -> ReadRequest -> SqlQuery
readRequestToQuery schema isParent (Node (Select colSelects tbl tblAlias implJoins logicForest joinConditions_ ordts range, _) forest) =
  unwords [
    "SELECT " <> intercalate ", " (map (pgFmtSelectItem qi) colSelects ++ selects),
    "FROM " <> intercalate ", " (tabl : implJs),
    unwords joins,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest ++ map pgFmtJoinCondition joinConditions_))
      `emptyOnFalse` (null logicForest && null joinConditions_),
    ("ORDER BY " <> intercalate ", " (map (pgFmtOrderTerm qi) ordts)) `emptyOnFalse` null ordts,
    ("LIMIT " <> maybe "ALL" show (rangeLimit range) <> " OFFSET " <> show (rangeOffset range)) `emptyOnFalse` (isParent || range == allRange) ]

  where
    implJs = fromQi . QualifiedIdentifier schema <$> implJoins
    mainQi = removeSourceCTESchema schema tbl
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
           where subquery = readRequestToQuery schema False (Node n forst)
    getQueryParts (Node n@(_, (name, Just Relation{relType=Parent,relTable=Table{tableName=table}}, alias, _, _)) forst) (j,s) = (joi:j,sel:s)
      where
        aliasOrName = fromMaybe name alias
        localTableName = pgFmtIdent $ table <> "_" <> aliasOrName
        sel = "row_to_json(" <> localTableName <> ".*) AS " <> pgFmtIdent aliasOrName
        joi = " LEFT JOIN LATERAL( " <> subquery <> " ) AS " <> localTableName <> " ON TRUE "
          where subquery = readRequestToQuery schema True (Node n forst)
    getQueryParts (Node n@(_, (name, Just Relation{relType=Many,relTable=Table{tableName=table}}, alias, _, _)) forst) (j,s) = (j,sel:s)
      where
        sel = "COALESCE (("
           <> "SELECT json_agg(" <> pgFmtIdent table <> ".*) "
           <> "FROM (" <> subquery <> ") " <> pgFmtIdent table
           <> "), '[]') AS " <> pgFmtIdent (fromMaybe name alias)
           where subquery = readRequestToQuery schema False (Node n forst)
    --the following is just to remove the warning
    --getQueryParts is not total but readRequestToQuery is called only after addJoinConditions which ensures the only
    --posible relations are Child Parent Many
    getQueryParts _ _ = witness


mutateRequestToQuery :: Schema -> MutateRequest -> SqlQuery
mutateRequestToQuery schema (Insert mainTbl iCols onConflct putConditions returnings) =
  unwords [
    "WITH " <> normalizedBody,
    "INSERT INTO ", fromQi qi, if S.null iCols then " " else "(" <> cols <> ")",
    unwords [
      "SELECT " <> cols <> " FROM",
      "json_populate_recordset", "(null::", fromQi qi, ", " <> selectBody <> ") _",
      -- Only used for PUT
      ("WHERE " <> intercalate " AND " (pgFmtLogicTree (QualifiedIdentifier "" "_") <$> putConditions)) `emptyOnFalse` null putConditions],
    maybe "" (\(oncDo, oncCols) -> (
      "ON CONFLICT(" <> intercalate ", " (pgFmtIdent <$> oncCols) <> ") " <> case oncDo of
      IgnoreDuplicates ->
        "DO NOTHING"
      MergeDuplicates  ->
        if S.null iCols
           then "DO NOTHING"
           else "DO UPDATE SET " <> intercalate ", " (pgFmtIdent <> const " = EXCLUDED." <> pgFmtIdent <$> S.toList iCols)
                                   ) `emptyOnFalse` null oncCols) onConflct,
    ("RETURNING " <> intercalate ", " (map (pgFmtColumn qi) returnings)) `emptyOnFalse` null returnings]
  where
    qi = QualifiedIdentifier schema mainTbl
    cols = intercalate ", " $ pgFmtIdent <$> S.toList iCols
mutateRequestToQuery schema (Update mainTbl uCols logicForest returnings) =
  if S.null uCols
    then "WITH " <> ignoredBody <> "SELECT null WHERE false" -- if there are no columns we cannot do UPDATE table SET {empty}, it'd be invalid syntax
    else
      unwords [
        "WITH " <> normalizedBody,
        "UPDATE " <> fromQi qi <> " SET " <> cols,
        "FROM (SELECT * FROM json_populate_recordset", "(null::", fromQi qi, ", " <> selectBody <> ")) _ ",
        ("WHERE " <> intercalate " AND " (pgFmtLogicTree qi <$> logicForest)) `emptyOnFalse` null logicForest,
        ("RETURNING " <> intercalate ", " (pgFmtColumn qi <$> returnings)) `emptyOnFalse` null returnings
        ]
  where
    qi = QualifiedIdentifier schema mainTbl
    cols = intercalate ", " (pgFmtIdent <> const " = _." <> pgFmtIdent <$> S.toList uCols)
mutateRequestToQuery schema (Delete mainTbl logicForest returnings) =
  unwords [
    "WITH " <> ignoredBody,
    "DELETE FROM ", fromQi qi,
    ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest,
    ("RETURNING " <> intercalate ", " (map (pgFmtColumn qi) returnings)) `emptyOnFalse` null returnings
    ]
  where
    qi = QualifiedIdentifier schema mainTbl

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
readRequestToCountQuery :: Schema -> ReadRequest -> SqlQuery
readRequestToCountQuery schema (Node (Select{where_=logicForest}, (mainTbl, _, _, _, _)) _) =
 unwords [
   "SELECT 1",
   "FROM " <> fromQi qi,
   ("WHERE " <> intercalate " AND " (map (pgFmtLogicTree qi) logicForest)) `emptyOnFalse` null logicForest
   ]
 where
   qi = removeSourceCTESchema schema mainTbl

limitedQuery :: SqlQuery -> Maybe Integer -> SqlQuery
limitedQuery query maxRows = query <> maybe mempty (\x -> " LIMIT " <> show x) maxRows

setLocalQuery :: Text -> (Text, Text) -> SqlQuery
setLocalQuery prefix (k, v) =
  "SET LOCAL " <> pgFmtIdent (prefix <> k) <> " = " <> pgFmtLit v <> ";"

setLocalSearchPathQuery :: [Text] -> SqlQuery
setLocalSearchPathQuery vals =
  "SET LOCAL search_path = " <> intercalate ", " (pgFmtLit <$> vals) <> ";"
