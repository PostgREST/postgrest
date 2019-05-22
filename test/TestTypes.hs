module TestTypes (
  IncPK(..)
, CompoundPK(..)
) where

import           Data.Aeson ((.:))
import qualified Data.Aeson as JSON

import Protolude

data IncPK = IncPK {
  incId          :: Int
, incNullableStr :: Maybe Text
, incStr         :: Text
, incInsert      :: Text
} deriving (Eq, Show)

instance JSON.FromJSON IncPK where
  parseJSON (JSON.Object r) = IncPK <$>
    r .: "id" <*>
    r .: "nullable_string" <*>
    r .: "non_nullable_string" <*>
    r .: "inserted_at"
  parseJSON _ = mzero

data CompoundPK = CompoundPK {
  compoundK1    :: Int
, compoundK2    :: Text
, compoundExtra :: Maybe Int
} deriving (Eq, Show)

instance JSON.FromJSON CompoundPK where
  parseJSON (JSON.Object r) = CompoundPK <$>
    r .: "k1" <*>
    r .: "k2" <*>
    r .: "extra"
  parseJSON _ = mzero
