{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types(SqlRow(SqlRow), getRow) where

import Database.HDBC (toSql, SqlValue(..))

import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)

import Data.Scientific (toRealFloat)
import Data.HashMap.Strict (foldlWithKey')
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (showGregorian)

import Control.Monad(mzero)

instance JSON.FromJSON SqlValue where
  parseJSON (JSON.String s)    = return $ toSql s
  parseJSON (JSON.Number n)    = return $ toSql (toRealFloat n::Double)
  parseJSON (JSON.Bool   b)    = return $ toSql b
  parseJSON JSON.Null          = return SqlNull
  parseJSON (JSON.Object o)    = return . toSql $ JSON.encode o
  parseJSON (JSON.Array  a)    = return . toSql $ JSON.encode a

instance JSON.ToJSON SqlValue where
  toJSON (SqlString s)         = JSON.toJSON s
  toJSON (SqlByteString s)     = JSON.toJSON $ decodeUtf8 s
  toJSON (SqlWord32 w)         = JSON.toJSON w
  toJSON (SqlWord64 w)         = JSON.toJSON w
  toJSON (SqlInt32 i)          = JSON.toJSON i
  toJSON (SqlInt64 i)          = JSON.toJSON i
  toJSON (SqlInteger i)        = JSON.toJSON i
  toJSON (SqlChar c)           = JSON.toJSON c
  toJSON (SqlBool b)           = JSON.toJSON b
  toJSON (SqlDouble n)         = JSON.toJSON n
  toJSON (SqlRational n)       = JSON.toJSON n
  toJSON (SqlLocalDate d)      = JSON.toJSON $ showGregorian d
  toJSON (SqlLocalTimeOfDay t) = JSON.toJSON $ show t
  toJSON (SqlLocalTime t)      = JSON.toJSON $ show t
  toJSON SqlNull               = JSON.Null
  toJSON x                     = JSON.toJSON $ show x


newtype SqlRow = SqlRow {getRow :: [(Text, SqlValue)] }
instance JSON.FromJSON SqlRow where
  parseJSON (JSON.Object m) = foldlWithKey' add (return $ SqlRow []) m
    where
      add :: Parser SqlRow -> Text -> JSON.Value -> Parser SqlRow
      add parser k v = do
        SqlRow l <- parser
        sqlV <- JSON.parseJSON v
        return . SqlRow $ (k, sqlV) : l
  parseJSON _ = mzero
