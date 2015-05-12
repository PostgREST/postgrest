module TestTypes (
  IncPK(..)
, CompoundPK(..)
-- , incFromList
-- , compoundFromList
) where

import qualified Data.Aeson as JSON
import Data.Aeson ((.:))
-- import Data.Maybe (fromJust)
import Control.Applicative
import Control.Monad (mzero)

import Prelude

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

-- incFromList :: [(String, SqlValue)] -> IncPK
-- incFromList row = IncPK
--   (fromSql . fromJust $ lookup "id" row)
--   (fromSql . fromJust $ lookup "nullable_string" row)
--   (fromSql . fromJust $ lookup "non_nullable_string" row)
--   (fromSql . fromJust $ lookup "inserted_at" row)

data CompoundPK = CompoundPK {
  compoundK1 :: Int
, compoundK2 :: Int
, compoundExtra :: Maybe Int
}

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
