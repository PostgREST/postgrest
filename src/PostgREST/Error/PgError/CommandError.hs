{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgREST.Error.PgError.CommandError where

import qualified Data.Aeson as JSON
import qualified Data.Text.Encoding as T
import qualified Hasql.Session as SQL
import qualified Network.HTTP.Types as HTTP
import PostgREST.Error.Algebra
import qualified PostgREST.Error.PgError.ServerError as ResultError
import Protolude

instance JSON.ToJSON SQL.CommandError where
  toJSON err =
    toJsonPgrstError
      (code err)
      (message err)
      (details err)
      (hint err)

instance ErrorBody SQL.CommandError where
  -- Special error raised with code PGRST, to allow full response control
  code (SQL.ResultError resultError) = code resultError
  code (SQL.ClientError _) = "PGRST001"

  message (SQL.ResultError resultError) = message resultError
  message (SQL.ClientError _) = "Database client error. Retrying the connection."

  details (SQL.ResultError resultError) = details resultError
  details (SQL.ClientError d) = JSON.String . T.decodeUtf8 <$> d

  hint (SQL.ResultError resultError) = hint resultError
  hint _ = Nothing

pgErrorStatus :: Bool -> SQL.CommandError -> HTTP.Status
pgErrorStatus _      (SQL.ClientError _)      = HTTP.status503
pgErrorStatus authed (SQL.ResultError rError) =
  ResultError.pgErrorStatus authed rError

maybeHeaders :: SQL.CommandError -> Maybe [HTTP.Header]
maybeHeaders (SQL.ResultError rError) = ResultError.maybeHeaders rError
maybeHeaders _ = Nothing
