{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgREST.Error.CommandError where

import qualified Data.Aeson as JSON
import qualified Data.Text.Encoding as T
import qualified Hasql.Session as SQL
import PostgREST.Error.Algebra
import PostgREST.Error.ResultError ()
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
