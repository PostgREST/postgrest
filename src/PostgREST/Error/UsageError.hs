{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgREST.Error.UsageError where

import qualified Data.Aeson                  as JSON
import qualified Data.Text.Encoding          as T
import qualified Hasql.Pool                  as SQL
import qualified Hasql.Session               as SQL

import PostgREST.Error.Algebra
import PostgREST.Error.CommandError ()
import Protolude


instance JSON.ToJSON SQL.UsageError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody SQL.UsageError where
  code    (SQL.ConnectionUsageError _)                   = "PGRST000"
  code    (SQL.SessionUsageError (SQL.QueryError _ _ e)) = code e
  code    SQL.AcquisitionTimeoutUsageError               = "PGRST003"

  message (SQL.ConnectionUsageError _) = "Database connection error. Retrying the connection."
  message (SQL.SessionUsageError (SQL.QueryError _ _ e)) = message e
  message SQL.AcquisitionTimeoutUsageError = "Timed out acquiring connection from connection pool."

  details (SQL.ConnectionUsageError e) = JSON.String . T.decodeUtf8 <$> e
  details (SQL.SessionUsageError (SQL.QueryError _ _ e)) = details e
  details SQL.AcquisitionTimeoutUsageError               = Nothing

  hint    (SQL.ConnectionUsageError _)                   = Nothing
  hint    (SQL.SessionUsageError (SQL.QueryError _ _ e)) = hint e
  hint    SQL.AcquisitionTimeoutUsageError               = Nothing
