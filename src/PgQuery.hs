{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

module PgQuery (
  getRows,
  insert,
  upsert,
  RangedResult(..),
) where

import Data.Text (Text, pack)
import Data.Functor ( (<$>) )
import Data.Maybe (fromMaybe)
import Data.List (intersperse, intercalate)
import Data.Monoid ((<>), mconcat)
import qualified Data.Map as M

import Control.Monad (join)

import qualified RangeQuery as R
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Database.HDBC hiding (colType, colNullable)
import Database.HDBC.PostgreSQL

import qualified Network.HTTP.Types.URI as Net

import Types (SqlRow, getRow, sqlRowColumns, sqlRowValues)

import Debug.Trace

-- }}}

data RangedResult = RangedResult {
  rrFrom  :: Int
, rrTo    :: Int
, rrTotal :: Int
, rrBody  :: BL.ByteString
} deriving (Show)

type QuotedSql = (String, [SqlValue])
type Schema = String

getRows :: Schema -> String -> Net.Query -> Maybe R.NonnegRange -> Connection -> IO RangedResult
getRows schema table qq range conn = do
  query <- populateSql conn
    $ globalAndLimitedCounts schema table qq <>
      jsonArrayRows
      (selectStarClause schema table
        <> whereClause qq
        <> limitClause range)
  r <- quickQuery conn query []

  return $ case r of
           [[total, _, SqlNull]] -> RangedResult offset 0 (fromSql total) ""
           [[total, limited_total, json]] ->
            RangedResult offset (offset + fromSql limited_total - 1)
                         (fromSql total) (fromSql json)
           _ -> RangedResult 0 0 0 ""

  where
    offset = fromMaybe 0 $ R.offset <$> range

whereClause :: Net.Query -> QuotedSql
whereClause qs =
  if null qs then ("", []) else (" where ", []) <> conjunction

  where
    conjunction = mconcat $ intersperse (" and ", []) (map wherePred qs)

wherePred :: Net.QueryItem -> QuotedSql
wherePred (column, predicate) =
  ("%I " <> op <> "%L", map toSql [column, value])

  where
    opCode:rest = BS.split '.' $ fromMaybe "." predicate
    value = BS.intercalate "." rest
    op = case opCode of
              "eq"  -> "="
              "gt"  -> ">"
              "lt"  -> "<"
              "gte" -> ">="
              "lte" -> "<="
              "neq" -> "<>"
              _     -> "="

limitClause :: Maybe R.NonnegRange -> QuotedSql
limitClause range =
  (" LIMIT %s OFFSET %s ", [toSql limit, toSql offset])

  where
    limit  = fromMaybe "ALL" $ show <$> (R.limit =<< range)
    offset = fromMaybe 0     $ R.offset <$> range

globalAndLimitedCounts :: Schema -> String -> Net.Query -> QuotedSql
globalAndLimitedCounts schema table qq =
  (" select ", [])
  <> ("(select count(1) from %I.%I ", map toSql [schema, table])
  <> whereClause qq
  <> ("), count(t), ", [])

selectStarClause :: Schema -> String -> QuotedSql
selectStarClause schema table =
  (" select * from %I.%I ", map toSql [schema, table])

-- selectCountClause :: Schema -> String -> QuotedSql
-- selectCountClause schema table =
--   (" select count(1) from %I.%I ", map toSql [schema, table])

jsonArrayRows :: QuotedSql -> QuotedSql
jsonArrayRows q =
  ("array_to_json(array_agg(row_to_json(t))) from (", []) <> q <> (") t", [])

insert :: Schema -> Text -> SqlRow -> Connection -> IO (M.Map String SqlValue)
insert schema table row conn = do
  sql    <- populateSql conn $ insertClause schema table row
  stmt   <- prepare conn sql
  _      <- execute stmt $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return m

upsert :: Schema -> Text -> SqlRow -> Net.Query -> Connection -> IO (M.Map String SqlValue)
upsert schema table row qq conn = do
  sql    <- populateSql conn $ upsertClause schema table row qq
  stmt   <- prepare conn (traceShow sql sql)
  _      <- execute stmt $ join $ replicate 2 $ sqlRowValues row
  Just m <- fetchRowMap stmt
  return (traceShow m m)

placeholders :: String -> SqlRow -> String
placeholders symbol = intercalate ", " . map (const symbol) . getRow

insertClause :: Schema -> Text -> SqlRow -> QuotedSql
insertClause schema table row =
    ("insert into %I.%I (" ++ placeholders "%I" row ++ ")",
     map toSql $ (pack schema) : table : sqlRowColumns row)
  <> (" values (" ++ placeholders "?" row ++ ") returning *", sqlRowValues row)


insertClauseViaSelect :: Schema -> Text -> SqlRow -> QuotedSql
insertClauseViaSelect schema table row =
    ("insert into %I.%I (" ++ placeholders "%I" row ++ ")",
     map toSql $ (pack schema) : table : sqlRowColumns row)
  <> (" select " ++ placeholders "?" row, sqlRowValues row)

updateClause :: Schema -> Text -> SqlRow -> QuotedSql
updateClause schema table row =
    ("update %I.%I set (" ++ placeholders "%I" row ++ ")",
     map toSql $ (pack schema) : table : sqlRowColumns row)
  <> (" = (" ++ placeholders "?" row ++ ")", [])

upsertClause :: Schema -> Text -> SqlRow -> Net.Query -> QuotedSql
upsertClause schema table row qq =
  ("with upsert as (", []) <> updateClause schema table row
  <> whereClause qq
  <> (" returning *) ", []) <> insertClauseViaSelect schema table row
  <> (" where not exists (select * from upsert) returning *", [])

populateSql :: Connection -> QuotedSql -> IO String
populateSql conn sql = do
  [[escaped]] <- quickQuery conn q (snd sql)
  return $ fromSql escaped

  where
    q = concat [ "select format('", fst sql, "', ", ph (snd sql), ")" ]

    ph :: [a] -> String
    ph = intercalate ", " . map (const "?::varchar")
