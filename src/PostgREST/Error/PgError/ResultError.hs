{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgREST.Error.PgError.ResultError
  ( toHttpStatusByAuthed,
    toHeaders,
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Internal as M
import qualified Data.Text.Encoding as T
import qualified Hasql.Session as SQL
import qualified Network.HTTP.Types as HTTP
import qualified PostgREST.Error.PgError.ResultError.RaisePgrst as RaisePgrst

import PostgREST.Error.Algebra
import Protolude

instance ErrorBody SQL.ResultError where
  -- Special error raised with code PGRST, to allow full response control
  code (SQL.ServerError "PGRST" m d _ _) =
    case RaisePgrst.parseRaisePGRST m d of
      Right (r, _) -> RaisePgrst.getCode r
      Left e       -> code e
  code (SQL.ServerError c _ _ _ _) = T.decodeUtf8 c
  code _ = "PGRSTX00" -- Internal Error

  message (SQL.ServerError "PGRST" m d _ _) =
    case RaisePgrst.parseRaisePGRST m d of
      Right (r, _) -> RaisePgrst.getMessage r
      Left e       -> message e
  message (SQL.ServerError _ m _ _ _) = T.decodeUtf8 m
  message resultError = show resultError -- We never really return this error, because we kill pgrst thread early in App.hs

  details (SQL.ServerError "PGRST" m d _ _) =
    case RaisePgrst.parseRaisePGRST m d of
      Right (r, _) -> JSON.String <$> RaisePgrst.getDetails r
      Left e       -> details e
  details (SQL.ServerError _ _ d _ _) = JSON.String . T.decodeUtf8 <$> d
  details _ = Nothing

  hint (SQL.ServerError "PGRST" m d _ _p) =
    case RaisePgrst.parseRaisePGRST m d of
      Right (r, _) -> JSON.String <$> RaisePgrst.getHint r
      Left e       -> hint e
  hint (SQL.ServerError _ _ _ h _) = JSON.String . T.decodeUtf8 <$> h
  hint _ = Nothing

toHttpStatusByAuthed :: SQL.ResultError -> Bool -> HTTP.Status
toHttpStatusByAuthed rError authed = case rError of
  SQL.ServerError c m d _ _ ->
    case BS.unpack c of
      '0':'8':_ -> HTTP.status503 -- pg connection err
      '0':'9':_ -> HTTP.status500 -- triggered action exception
      '0':'L':_ -> HTTP.status403 -- invalid grantor
      '0':'P':_ -> HTTP.status403 -- invalid role specification
      "23503"   -> HTTP.status409 -- foreign_key_violation
      "23505"   -> HTTP.status409 -- unique_violation
      "25006"   -> HTTP.status405 -- read_only_sql_transaction
      "21000"   -> -- cardinality_violation
        if BS.isSuffixOf "requires a WHERE clause" m
          then HTTP.status400 -- special case for pg-safeupdate, which we consider as client error
          else HTTP.status500 -- generic function or view server error, e.g. "more than one row returned by a subquery used as an expression"
      "22023"   -> -- invalid_parameter_value. Catch nonexistent role error, see https://github.com/PostgREST/postgrest/issues/3601
        if BS.isPrefixOf "role" m && BS.isSuffixOf "does not exist" m
          then HTTP.status401 -- role in jwt does not exist
          else HTTP.status400
      '2':'5':_ -> HTTP.status500 -- invalid tx state
      '2':'8':_ -> HTTP.status403 -- invalid auth specification
      '2':'D':_ -> HTTP.status500 -- invalid tx termination
      '3':'8':_ -> HTTP.status500 -- external routine exception
      '3':'9':_ -> HTTP.status500 -- external routine invocation
      '3':'B':_ -> HTTP.status500 -- savepoint exception
      '4':'0':_ -> HTTP.status500 -- tx rollback
      "53400"   -> HTTP.status500 -- config limit exceeded
      '5':'3':_ -> HTTP.status503 -- insufficient resources
      '5':'4':_ -> HTTP.status500 -- too complex
      '5':'5':_ -> HTTP.status500 -- obj not on prereq state
      "57P01"   -> HTTP.status503 -- terminating connection due to administrator command
      '5':'7':_ -> HTTP.status500 -- operator intervention
      '5':'8':_ -> HTTP.status500 -- system error
      'F':'0':_ -> HTTP.status500 -- conf file error
      'H':'V':_ -> HTTP.status500 -- foreign data wrapper error
      "P0001"   -> HTTP.status400 -- default code for "raise"
      'P':'0':_ -> HTTP.status500 -- PL/pgSQL Error
      'X':'X':_ -> HTTP.status500 -- internal Error
      "42883"-> if BS.isPrefixOf "function xmlagg(" m
        then HTTP.status406
        else HTTP.status404 -- undefined function
      "42P01"   -> HTTP.status404 -- undefined table
      "42P17"   -> HTTP.status500 -- infinite recursion
      "42501"   -> if authed then HTTP.status403 else HTTP.status401 -- insufficient privilege
      'P':'T':n -> fromMaybe HTTP.status500 (HTTP.mkStatus <$> readMaybe n <*> pure m)
      "PGRST"   ->
        case RaisePgrst.parseRaisePGRST m d of
          Right (_, r) -> maybe (toEnum $ RaisePgrst.getStatus r) (HTTP.mkStatus (RaisePgrst.getStatus r) . T.encodeUtf8) (RaisePgrst.getStatusText r)
          Left e -> status e
      _         -> HTTP.status400
  _             -> HTTP.status500

toHeaders :: SQL.ResultError -> Maybe [HTTP.Header]
toHeaders (SQL.ServerError "PGRST" m d _ _p) =
  Just $ case RaisePgrst.parseRaisePGRST m d of
    Right (_, r) -> map intoHeader (M.toList $ RaisePgrst.getHeaders r)
    Left e       -> headers e
  where
    intoHeader (k,v) = (CI.mk $ T.encodeUtf8 k, T.encodeUtf8 v)
toHeaders _ = Nothing
