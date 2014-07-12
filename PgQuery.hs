{-# LANGUAGE OverloadedStrings #-}

module PgQuery where

import Data.Functor ( (<$>) )
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Monoid ((<>))

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import Database.HDBC hiding (colType, colNullable)
import Database.HDBC.PostgreSQL

import Network.HTTP.Types.URI

selectWhere :: T.Text -> Query -> Connection -> IO BL.ByteString
selectWhere table qq conn = do
  s <- selectSql
  w <- whereClause conn qq
  r <- quickQuery conn (BS.unpack $ s <> w) []
  return $ case r of
                [[json]] -> fromSql json
                _        -> "" :: BL.ByteString

  where
    selectSql = pgFormat conn
          "select array_to_json(array_agg(row_to_json(t)))\
          \  from (select * from %I.%I) t"
        [toSql (T.pack "base"), toSql table]


whereClause :: Connection -> Query -> IO BS.ByteString
whereClause _    [] = return ""
whereClause conn qs =
  (" where " <>) <$> clause

  where
    clause :: IO BS.ByteString
    clause = BS.intercalate " and " <$> preds

    preds :: IO [BS.ByteString]
    preds = sequence $ map (wherePred conn) qs


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
