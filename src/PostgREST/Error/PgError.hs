{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module PostgREST.Error.PgError
  ( PgError(..),
    Authenticated,
  ) where

import qualified Data.Aeson                          as JSON
import qualified Hasql.Pool                          as SQL
import qualified Hasql.Session                       as SQL
import qualified Network.HTTP.Types                  as HTTP
import qualified PostgREST.Error.PgError.ResultError as ResultError

import PostgREST.Error.Algebra
import PostgREST.Error.PgError.CommandError ()
import PostgREST.Error.PgError.ResultError ()
import PostgREST.Error.PgError.UsageError ()
import Protolude

data PgError = PgError Authenticated SQL.UsageError
  deriving Show

type Authenticated = Bool

instance PgrstError PgError where
  status (PgError authed usageError) = pgErrorStatus authed usageError

  headers (PgError _ (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError (ResultError.toHeaders -> Just matchingHeaders))))) =
    matchingHeaders

  headers err =
    if status err == HTTP.status401
       then [("WWW-Authenticate", "Bearer") :: HTTP.Header]
       else mempty

instance JSON.ToJSON PgError where
  toJSON (PgError _ usageError) = toJsonPgrstError
    (code usageError) (message usageError) (details usageError) (hint usageError)

instance ErrorBody PgError where
  code    (PgError _ usageError) = code usageError
  message (PgError _ usageError) = message usageError
  details (PgError _ usageError) = details usageError
  hint    (PgError _ usageError) = hint usageError

pgErrorStatus :: Bool -> SQL.UsageError -> HTTP.Status
pgErrorStatus _      (SQL.ConnectionUsageError _) = HTTP.status503
pgErrorStatus _      SQL.AcquisitionTimeoutUsageError = HTTP.status504
pgErrorStatus _      (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ClientError _)))      = HTTP.status503
pgErrorStatus authed (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError rError))) =
  ResultError.toHttpStatusByAuthed rError authed
