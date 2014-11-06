module TestTypes (
  IncPK(..),
  fromList
) where

import qualified Data.Aeson as JSON
import Data.Aeson ((.:))
import Data.Maybe (fromJust)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Database.HDBC (SqlValue, fromSql)

data IncPK = IncPK {
  incId :: Int
, incNullableStr :: Maybe String
, incStr :: String
, incInsert :: String
} deriving (Eq, Show)

instance JSON.FromJSON IncPK where
  parseJSON (JSON.Object r) = IncPK <$>
    r .: "id" <*>
    r .: "nullable_string" <*>
    r .: "non_nullable_string" <*>
    r .: "inserted_at"
  parseJSON _ = mzero

fromList :: [(String, SqlValue)] -> IncPK
fromList row = IncPK
  (fromSql . fromJust $ lookup "id" row)
  (fromSql . fromJust $ lookup "nullable_string" row)
  (fromSql . fromJust $ lookup "non_nullable_string" row)
  (fromSql . fromJust $ lookup "inserted_at" row)
