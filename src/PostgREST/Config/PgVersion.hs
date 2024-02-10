{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module PostgREST.Config.PgVersion
  ( PgVersion(..)
  , minimumPgVersion
  , pgVersion120
  , pgVersion121
  , pgVersion130
  , pgVersion140
  , pgVersion150
  ) where

import qualified Data.Aeson as JSON

import Protolude


data PgVersion = PgVersion
  { pgvNum      :: Int32
  , pgvName     :: Text
  , pgvFullName :: Text
  }
  deriving (Eq, Generic, JSON.ToJSON)

instance Ord PgVersion where
  (PgVersion v1 _ _) `compare` (PgVersion v2 _ _) = v1 `compare` v2

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: PgVersion
minimumPgVersion = pgVersion11

-- PostgreSQL 11 is EOL already, so we only allow the last
-- minor release as the minimum version. Theoretically. But
-- the version we are using from legacy nix is only 11.21,
-- so we are happy with that.
pgVersion11 :: PgVersion
pgVersion11 = PgVersion 110021 "11.21" "11.21"

pgVersion120 :: PgVersion
pgVersion120 = PgVersion 120000 "12.0" "12.0"

pgVersion121 :: PgVersion
pgVersion121 = PgVersion 120001 "12.1" "12.1"

pgVersion130 :: PgVersion
pgVersion130 = PgVersion 130000 "13.0" "13.0"

pgVersion140 :: PgVersion
pgVersion140 = PgVersion 140000 "14.0" "14.0"

pgVersion150 :: PgVersion
pgVersion150 = PgVersion 150000 "15.0" "15.0"
