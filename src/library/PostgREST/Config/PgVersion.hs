{-# LANGUAGE DeriveAnyClass #-}

module PostgREST.Config.PgVersion
  ( PgVersion (..)
  , minimumPgVersion
  , pgVersion150
  , pgVersion160
  , pgVersion170
  , pgVersion180
  , pgVersion190
  , pgVersion186
  , pgVersion1711
  , pgVersion1615
  , pgVersion1519
  , pgVersion1424
  , timezoneGucInvalid
  )
where

import Protolude

import Data.Aeson qualified as JSON

data PgVersion = PgVersion
  { pgvNum :: Int32,
    pgvName :: Text,
    pgvFullName :: Text
  }
  deriving (Eq, Generic, JSON.ToJSON, Show)

instance Ord PgVersion where
  (PgVersion v1 _ _) `compare` (PgVersion v2 _ _) = v1 `compare` v2

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: PgVersion
minimumPgVersion = pgVersion140

pgVersion140 :: PgVersion
pgVersion140 = PgVersion 140000 "14.0" "14.0"

pgVersion150 :: PgVersion
pgVersion150 = PgVersion 150000 "15.0" "15.0"

pgVersion160 :: PgVersion
pgVersion160 = PgVersion 160000 "16.0" "16.0"

pgVersion170 :: PgVersion
pgVersion170 = PgVersion 170000 "17.0" "17.0"

pgVersion180 :: PgVersion
pgVersion180 = PgVersion 180000 "18.0" "18.0"

pgVersion190 :: PgVersion
pgVersion190 = PgVersion 190000 "19.0" "19.0"

-- to_char/timezone interaction vulnerability fixed in below versions. See: https://www.postgresql.org/support/security/CVE-2026-14669/
pgVersion186 :: PgVersion
pgVersion186 = PgVersion 180006 "18.6" "18.6"

pgVersion1711 :: PgVersion
pgVersion1711 = PgVersion 170011 "17.11" "17.11"

pgVersion1615 :: PgVersion
pgVersion1615 = PgVersion 160015 "16.15" "16.15"

pgVersion1519 :: PgVersion
pgVersion1519 = PgVersion 150019 "15.19" "15.19"

pgVersion1424 :: PgVersion
pgVersion1424 = PgVersion 140024 "14.24" "14.24"

timezoneGucInvalid :: PgVersion -> Bool
timezoneGucInvalid currVer = any inRange validationRanges
  where
    inRange (major, patched) = currVer >= major && currVer < patched
    validationRanges =
      [ (pgVersion180, pgVersion186),
        (pgVersion170, pgVersion1711),
        (pgVersion160, pgVersion1615),
        (pgVersion150, pgVersion1519),
        (pgVersion140, pgVersion1424)
      ]
