{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module PostgREST.PgVersions
  ( PgVersion(..)
  , minimumPgVersion
  , pgVersion95
  , pgVersion96
  , pgVersion100
  , pgVersion109
  , pgVersion110
  , pgVersion112
  , pgVersion114
  , pgVersion121
  , pgVersion130
  ) where

import qualified Data.Aeson as JSON

import Protolude


data PgVersion = PgVersion
  { pgvNum  :: Int32
  , pgvName :: Text
  }
  deriving (Eq, Generic, JSON.ToJSON)

instance Ord PgVersion where
  (PgVersion v1 _) `compare` (PgVersion v2 _) = v1 `compare` v2

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: PgVersion
minimumPgVersion = pgVersion95

pgVersion95 :: PgVersion
pgVersion95 = PgVersion 90500 "9.5"

pgVersion96 :: PgVersion
pgVersion96 = PgVersion 90600 "9.6"

pgVersion100 :: PgVersion
pgVersion100 = PgVersion 100000 "10"

pgVersion109 :: PgVersion
pgVersion109 = PgVersion 100009 "10.9"

pgVersion110 :: PgVersion
pgVersion110 = PgVersion 110000 "11.0"

pgVersion112 :: PgVersion
pgVersion112 = PgVersion 110002 "11.2"

pgVersion114 :: PgVersion
pgVersion114 = PgVersion 110004 "11.4"

pgVersion121 :: PgVersion
pgVersion121 = PgVersion 120001 "12.1"

pgVersion130 :: PgVersion
pgVersion130 = PgVersion 130000 "13.0"
