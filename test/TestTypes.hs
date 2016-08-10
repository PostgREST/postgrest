module TestTypes (
  IncPK(..)
, CompoundPK(..)
) where

import qualified Data.Aeson as JSON
import Data.Aeson ((.:))

import Protolude

data IncPK = IncPK {
  incId :: Int
, incNullableStr :: Maybe Text
, incStr :: Text
, incInsert :: Text
} deriving (Eq, Show)

instance JSON.FromJSON IncPK where
  parseJSON (JSON.Object r) = IncPK <$>
    r .: "id" <*>
    r .: "nullable_string" <*>
    r .: "non_nullable_string" <*>
    r .: "inserted_at"
  parseJSON _ = mzero

-- incFromList :: [(String, SqlValue)] -> IncPK
-- incFromList row = IncPK
--   (fromSql . fromJust $ lookup "id" row)
--   (fromSql . fromJust $ lookup "nullable_string" row)
--   (fromSql . fromJust $ lookup "non_nullable_string" row)
--   (fromSql . fromJust $ lookup "inserted_at" row)

data CompoundPK = CompoundPK {
  compoundK1 :: Int
, compoundK2 :: Text
, compoundExtra :: Maybe Int
} deriving (Eq, Show)

instance JSON.FromJSON CompoundPK where
  parseJSON (JSON.Object r) = CompoundPK <$>
    r .: "k1" <*>
    r .: "k2" <*>
    r .: "extra"
  parseJSON _ = mzero

-- compoundFromList :: [(String, SqlValue)] -> CompoundPK
-- compoundFromList row = CompoundPK
--   (fromSql . fromJust $ lookup "k1" row)
--   (fromSql . fromJust $ lookup "k2" row)
--   (fromSql . fromJust $ lookup "extra" row)
