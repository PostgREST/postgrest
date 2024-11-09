{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module PostgREST.Config.PgVersion
  ( PgVersion(..)
  , minimumPgVersion
  , pgVersion130
  , pgVersion140
  , pgVersion150
  , pgVersion170
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
minimumPgVersion = pgVersion121

pgVersion121 :: PgVersion
pgVersion121 = PgVersion 120001 "12.1" "12.1"

pgVersion130 :: PgVersion
pgVersion130 = PgVersion 130000 "13.0" "13.0"

pgVersion140 :: PgVersion
pgVersion140 = PgVersion 140000 "14.0" "14.0"

pgVersion150 :: PgVersion
pgVersion150 = PgVersion 150000 "15.0" "15.0"

pgVersion170 :: PgVersion
pgVersion170 = PgVersion 170000 "17.0" "17.0"
