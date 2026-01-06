{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgREST.Error.PgError.UsageError where

import qualified Data.Aeson                          as JSON
import qualified Hasql.Pool                          as SQL
import qualified Hasql.Errors                        as SQL
import qualified Network.HTTP.Types                  as HTTP
import qualified PostgREST.Error.PgError.ServerError as ServerError

import PostgREST.Error.Algebra
import Protolude


instance JSON.ToJSON SQL.UsageError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody SQL.UsageError where
  code    (SQL.ConnectionUsageError _) = "PGRST000"
  code    SQL.AcquisitionTimeoutUsageError = "PGRST003"
  code    (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ (SQL.ServerStatementError serverError))) = code serverError
  code    (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ _)) = "PGRSTX00"
  code    (SQL.SessionUsageError (SQL.ScriptSessionError _ serverError)) = code serverError
  code    (SQL.SessionUsageError (SQL.ConnectionSessionError _)) = "PGRST001"
  code    (SQL.SessionUsageError (SQL.DriverSessionError _)) = "PGRST001"
  code    (SQL.SessionUsageError (SQL.MissingTypesSessionError _)) = "PGRSTX00"

  message (SQL.ConnectionUsageError _) = "Database connection error. Retrying the connection."
  message SQL.AcquisitionTimeoutUsageError = "Timed out acquiring connection from connection pool."
  message (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ (SQL.ServerStatementError serverError))) = message serverError
  message (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ _)) = "Result processing error."
  message (SQL.SessionUsageError (SQL.ScriptSessionError _ serverError)) = message serverError
  message (SQL.SessionUsageError (SQL.ConnectionSessionError _)) = "Database connection error. Retrying the connection."
  message (SQL.SessionUsageError (SQL.DriverSessionError _)) = "Database client error. Retrying the connection."
  message (SQL.SessionUsageError (SQL.MissingTypesSessionError _)) = "Missing named types requested."

  details (SQL.ConnectionUsageError e) = Just (JSON.String (SQL.toMessage e))
  details SQL.AcquisitionTimeoutUsageError = Nothing
  details (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ (SQL.ServerStatementError serverError))) = details serverError
  details (SQL.SessionUsageError (SQL.ScriptSessionError _ serverError)) = details serverError
  details (SQL.SessionUsageError sessionError) = Just (JSON.String (SQL.toMessage sessionError))

  hint    (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ (SQL.ServerStatementError serverError))) =
    hint serverError
  hint    (SQL.SessionUsageError (SQL.ScriptSessionError _ serverError)) =
    hint serverError
  hint    _ = Nothing

pgErrorStatus :: Bool -> SQL.UsageError -> HTTP.Status
pgErrorStatus _      (SQL.ConnectionUsageError _) = HTTP.status503
pgErrorStatus _      SQL.AcquisitionTimeoutUsageError = HTTP.status504
pgErrorStatus authed (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ (SQL.ServerStatementError serverError))) =
  ServerError.pgErrorStatus authed serverError
pgErrorStatus authed (SQL.SessionUsageError (SQL.ScriptSessionError _ serverError)) =
  ServerError.pgErrorStatus authed serverError
pgErrorStatus _      (SQL.SessionUsageError _) = HTTP.status503

maybeHeaders :: SQL.UsageError -> Maybe [HTTP.Header]
maybeHeaders (SQL.SessionUsageError (SQL.StatementSessionError _ _ _ _ _ (SQL.ServerStatementError serverError))) =
  ServerError.maybeHeaders serverError
maybeHeaders _ = Nothing
