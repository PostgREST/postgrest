{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module PostgREST.Config.PgVersion
  ( PgVersion(..)
  , minimumPgVersion
  , pgVersion109
  , pgVersion110
  , pgVersion112
  , pgVersion114
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
minimumPgVersion = pgVersion100

pgVersion100 :: PgVersion
pgVersion100 = PgVersion 100000 "10" "10"

pgVersion109 :: PgVersion
pgVersion109 = PgVersion 100009 "10.9" "10.9"

pgVersion110 :: PgVersion
pgVersion110 = PgVersion 110000 "11.0" "11.0"

pgVersion112 :: PgVersion
pgVersion112 = PgVersion 110002 "11.2" "11.2"

pgVersion114 :: PgVersion
pgVersion114 = PgVersion 110004 "11.4" "11.4"

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
