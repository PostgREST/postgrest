{-# LANGUAGE OverloadedStrings #-}

module PgStructure where

import Data.Functor ( (<$>) )
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intercalate)
import Data.Monoid ((<>))

import Data.HashMap.Strict hiding (map)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import qualified Data.Aeson as JSON

import Database.HDBC hiding (colType, colNullable)
import Database.HDBC.PostgreSQL

import Data.Aeson ((.=))
import Network.HTTP.Types.URI

import Debug.Trace

data Table = Table {
  tableSchema :: String
, tableName :: String
, tableInsertable :: Bool
} deriving (Show)

instance JSON.ToJSON Table where
  toJSON v = JSON.object [
      "schema"     .= tableSchema v
    , "name"       .= tableName v
    , "insertable" .= tableInsertable v ]

toBool :: String -> Bool
toBool = (== "YES")

data Column = Column {
  colSchema :: String
, colTable :: String
, colName :: String
, colPosition :: Int
, colNullable :: Bool
, colType :: String
, colUpdatable :: Bool
, colMaxLen :: Maybe Int
, colPrecision :: Maybe Int
} deriving (Show)

instance JSON.ToJSON Column where
  toJSON c = JSON.object [
      "schema"    .= colSchema c
    , "name"      .= colName c
    , "position"  .= colPosition c
    , "nullable"  .= colNullable c
    , "type"      .= colType c
    , "updatable" .= colUpdatable c
    , "maxLen"    .= colMaxLen c
    , "precision" .= colPrecision c ]

tables :: String -> Connection -> IO [Table]
tables s conn = do
  r <- quickQuery conn
        "select table_schema, table_name,\
        \       is_insertable_into\
        \  from information_schema.tables\
        \ where table_schema = ?" [toSql s]
  return $ mapMaybe mkTable r

  where
    mkTable [schema, name, insertable] =
      Just $ Table (fromSql schema)
        (fromSql name)
        (toBool (fromSql insertable))
    mkTable _ = Nothing

columns :: T.Text -> Connection -> IO [Column]
columns t conn = do
  r <- quickQuery conn
        "select table_schema, table_name, column_name, ordinal_position,\
        \       is_nullable, data_type, is_updatable,\
        \       character_maximum_length, numeric_precision\
        \  from information_schema.columns\
        \ where table_name = ?" [toSql t]
  return $ mapMaybe mkColumn r

  where
    mkColumn [schema, table, name, pos, nullable, colT, updatable, maxlen, precision] =
      Just $ Column (fromSql schema)
        (fromSql table)
        (fromSql name)
        (fromSql pos)
        (toBool (fromSql nullable))
        (fromSql colT)
        (toBool (fromSql updatable))
        (fromSql maxlen)
        (fromSql precision)
    mkColumn _ = Nothing

namedColumnHash :: [Column] -> HashMap String Column
namedColumnHash = fromList . (Prelude.zip =<< Prelude.map colName)

printTables :: Connection -> IO BL.ByteString
printTables conn = JSON.encode <$> tables "base" conn

printColumns :: T.Text -> Connection -> IO BL.ByteString
printColumns table conn = JSON.encode . namedColumnHash <$> columns table conn

traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x

selectWhere :: T.Text -> Query -> Connection -> IO BL.ByteString
selectWhere table qq conn = do
  let sql = unwords [
        "select array_to_json(array_agg(row_to_json(t)))\
        \  from (select * from %I.%I) t", whereClause qq ]
  statement <- prepareDynamic conn sql
     [toSql (T.pack "base"), toSql table]
  _ <- execute statement []
  r <- fetchAllRows statement
  return $ case r of
                [[json]] -> fromSql json
                _        -> "" :: BL.ByteString

  where
    wherePred :: QueryItem -> BS.ByteString
    wherePred (column, predicate) =
      let opCode:rest = BS.split ':' $ fromMaybe "" predicate
          value = BS.intercalate ":" rest
          op = case opCode of
                    "eq"  -> "="
                    "gt"  -> ">"
                    "lt"  -> "<"
                    "gte" -> ">="
                    "lte" -> "<="
                    "neq" -> "<>"
                    _     -> "="
       in BS.intercalate " " ["t." <> column, op, value]

    whereClause :: Query -> String
    whereClause [] = ""
    whereClause qs = BS.unpack $ "where " <> BS.intercalate " and " (map wherePred qs)

prepareDynamic :: Connection -> String -> [SqlValue] -> IO Statement
prepareDynamic conn sql args = do
  [[escaped]] <- quickQuery conn q args

  prepare conn $ fromSql escaped

  where
    q = concat [ "select format('", sql, "', ", placeholders args, ")" ]

    placeholders :: [a] -> String
    placeholders = intercalate ", " . map (const "?::varchar")
