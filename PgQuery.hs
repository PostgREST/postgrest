{-# LANGUAGE OverloadedStrings #-}

module PgQuery where

import Data.Functor ( (<$>) )
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Monoid ((<>))

import qualified RangeQuery as R
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import Database.HDBC hiding (colType, colNullable)
import Database.HDBC.PostgreSQL

import Network.HTTP.Types.URI

data RangedResult = RangedResult {
  rrFrom  :: Int
, rrTo    :: Int
, rrTotal :: Int
, rrBody  :: BL.ByteString
}

selectWhere :: T.Text -> T.Text -> Query -> Maybe R.NonnegRange -> Connection -> IO BL.ByteString
selectWhere ver table qq range conn = do
  s <- selectSql
  w <- whereClause conn qq
  r <- quickQuery conn (BS.unpack $ s <> w) []

  let body = case r of
             [[SqlNull]] -> "[]"::BL.ByteString
             [[json]] -> fromSql json
             _        -> "" :: BL.ByteString
  return body

  where
    limit = fromMaybe "ALL" $ show <$> (R.limit =<< range)
    offset = fromMaybe 0 (R.offset <$> range)
    selectSql = pgFormat conn
          "select array_to_json(array_agg(row_to_json(t)))\
          \  from (select * from %I.%I LIMIT %s OFFSET %s) t"
        [toSql ver, toSql table, toSql limit, toSql offset]


whereClause :: Connection -> Query -> IO BS.ByteString
whereClause _    [] = return ""
whereClause conn qs =
  (" where " <>) <$> clause

  where
    clause :: IO BS.ByteString
    clause = BS.intercalate " and " <$> preds

    preds :: IO [BS.ByteString]
    preds = mapM (wherePred conn) qs


wherePred :: Connection -> QueryItem -> IO BS.ByteString
wherePred conn (column, predicate) =
  pgFormat conn ("t.%I " <> op <> "%L") $ map toSql [column, value]

  where
    opCode:rest = BS.split ':' $ fromMaybe "" predicate
    value = BS.intercalate ":" rest
    op = case opCode of
              "eq"  -> "="
              "gt"  -> ">"
              "lt"  -> "<"
              "gte" -> ">="
              "lte" -> "<="
              "neq" -> "<>"
              _     -> "="


pgFormat :: Connection -> String -> [SqlValue] -> IO BS.ByteString
pgFormat conn sql args = do
  [[escaped]] <- quickQuery conn q args
  return $ fromSql escaped

  where
    q = concat [ "select format('", sql, "', ", placeholders args, ")" ]

    placeholders :: [a] -> String
    placeholders = intercalate ", " . map (const "?::varchar")
