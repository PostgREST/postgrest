{-|
Module      : PostgREST.Error
Description : PostgREST error HTTP responses
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module PostgREST.Error
  ( errorResponseFor
  , ApiRequestError(..)
  , QPError(..)
  , RangeError(..)
  , SchemaCacheError(..)
  , PgError(..)
  , Error(..)
  , JwtError (..)
  , JwtDecodeError(..)
  , JwtClaimsError(..)
  , errorPayload
  , status
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.CaseInsensitive      as CI
import qualified Data.Map.Internal         as M
import qualified Data.Text.Encoding        as T
import qualified Hasql.Pool                as SQL
import qualified Hasql.Session             as SQL
import qualified Network.HTTP.Types.Status as HTTP

import Data.Aeson  ((.:), (.:?))
import Network.HTTP.Types.Header (Header)
import PostgREST.Error.Algebra
import PostgREST.Error.ApiRequestError
import PostgREST.Error.SchemaCacheError
import Protolude


data PgError = PgError Authenticated SQL.UsageError
  deriving Show

type Authenticated = Bool

instance PgrstError PgError where
  status (PgError authed usageError) = pgErrorStatus authed usageError

  headers (PgError _ (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError (SQL.ServerError "PGRST" m d _ _p))))) =
    case parseRaisePGRST m d of
      Right (_, r) -> map intoHeader (M.toList $ getHeaders r)
      Left e       -> headers e
    where
      intoHeader (k,v) = (CI.mk $ T.encodeUtf8 k, T.encodeUtf8 v)

  headers err =
    if status err == HTTP.status401
       then [("WWW-Authenticate", "Bearer") :: Header]
       else mempty

proxyStatusHeader :: Text -> Header
proxyStatusHeader code' = ("Proxy-Status", "PostgREST; error=" <> T.encodeUtf8 code')

instance JSON.ToJSON PgError where
  toJSON (PgError _ usageError) = toJsonPgrstError
    (code usageError) (message usageError) (details usageError) (hint usageError)

instance ErrorBody PgError where
  code    (PgError _ usageError) = code usageError
  message (PgError _ usageError) = message usageError
  details (PgError _ usageError) = details usageError
  hint    (PgError _ usageError) = hint usageError

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

instance JSON.ToJSON SQL.CommandError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody SQL.CommandError where
  -- Special error raised with code PGRST, to allow full response control
  code (SQL.ResultError (SQL.ServerError "PGRST" m d _ _)) =
    case parseRaisePGRST m d of
      Right (r, _) -> getCode r
      Left e       -> code e
  code (SQL.ResultError (SQL.ServerError c _ _ _ _)) = T.decodeUtf8 c

  code (SQL.ResultError _) = "PGRSTX00" -- Internal Error

  code (SQL.ClientError _) = "PGRST001"

  message (SQL.ResultError (SQL.ServerError "PGRST" m d _ _)) =
    case parseRaisePGRST m d of
      Right (r, _) -> getMessage r
      Left e       -> message e
  message (SQL.ResultError (SQL.ServerError _ m _ _ _)) = T.decodeUtf8 m
  message (SQL.ResultError resultError) = show resultError -- We never really return this error, because we kill pgrst thread early in App.hs
  message (SQL.ClientError _) = "Database client error. Retrying the connection."

  details (SQL.ResultError (SQL.ServerError "PGRST" m d _ _)) =
    case parseRaisePGRST m d of
      Right (r, _) -> JSON.String <$> getDetails r
      Left e       -> details e
  details (SQL.ResultError (SQL.ServerError _ _ d _ _)) = JSON.String . T.decodeUtf8 <$> d
  details (SQL.ClientError d) = JSON.String . T.decodeUtf8 <$> d

  details _ = Nothing

  hint (SQL.ResultError (SQL.ServerError "PGRST" m d _ _p)) =
    case parseRaisePGRST m d of
      Right (r, _) -> JSON.String <$> getHint r
      Left e       -> hint e
  hint (SQL.ResultError (SQL.ServerError _ _ _ h _)) = JSON.String . T.decodeUtf8 <$> h

  hint _                   = Nothing


pgErrorStatus :: Bool -> SQL.UsageError -> HTTP.Status
pgErrorStatus _      (SQL.ConnectionUsageError _) = HTTP.status503
pgErrorStatus _      SQL.AcquisitionTimeoutUsageError = HTTP.status504
pgErrorStatus _      (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ClientError _)))      = HTTP.status503
pgErrorStatus authed (SQL.SessionUsageError (SQL.QueryError _ _ (SQL.ResultError rError))) =
  case rError of
    (SQL.ServerError c m d _ _) ->
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
          case parseRaisePGRST m d of
            Right (_, r) -> maybe (toEnum $ getStatus r) (HTTP.mkStatus (getStatus r) . T.encodeUtf8) (getStatusText r)
            Left e       -> status e
        _         -> HTTP.status400

    _                       -> HTTP.status500


data Error
  = ApiRequestError ApiRequestError
  | SchemaCacheErr SchemaCacheError
  | JwtErr JwtError
  | NoSchemaCacheError
  | PgErr PgError
  deriving Show

data JwtError
  = JwtDecodeErr JwtDecodeError
  | JwtSecretMissing
  | JwtTokenRequired
  | JwtClaimsErr JwtClaimsError
  deriving Show

data JwtDecodeError
  = EmptyAuthHeader
  | UnexpectedParts Int
  | KeyError Text
  | BadAlgorithm Text
  | BadCrypto
  | UnsupportedTokenType
  | UnreachableDecodeError
  deriving Show

data JwtClaimsError
  = JWTExpired
  | JWTNotYetValid
  | JWTIssuedAtFuture
  | JWTNotInAudience
  | ParsingClaimsFailed
  | ExpClaimNotNumber
  | NbfClaimNotNumber
  | IatClaimNotNumber
  | AudClaimNotStringOrArray
  deriving Show

instance PgrstError Error where
  status (ApiRequestError err) = status err
  status (SchemaCacheErr err)  = status err
  status (JwtErr err)          = status err
  status NoSchemaCacheError    = HTTP.status503
  status (PgErr err)           = status err

  headers (ApiRequestError err)  = proxyStatusHeader (code err) : headers err
  headers (SchemaCacheErr err)   = proxyStatusHeader (code err) : headers err
  headers (JwtErr err)           = proxyStatusHeader (code err) : headers err
  headers (PgErr err)            = proxyStatusHeader (code err) : headers err
  headers err@NoSchemaCacheError = proxyStatusHeader (code err) : mempty

instance JSON.ToJSON Error where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody Error where
  code (ApiRequestError err) = code err
  code (SchemaCacheErr err)  = code err
  code (JwtErr err)          = code err
  code NoSchemaCacheError    = "PGRST002"
  code (PgErr err)           = code err

  message (ApiRequestError err) = message err
  message (SchemaCacheErr err)  = message err
  message (JwtErr err)          = message err
  message NoSchemaCacheError    = "Could not query the database for the schema cache. Retrying."
  message (PgErr err)           = message err

  details (ApiRequestError err) = details err
  details (SchemaCacheErr err)  = details err
  details (JwtErr err)          = details err
  details NoSchemaCacheError    = Nothing
  details (PgErr err)           = details err

  hint (ApiRequestError err) = hint err
  hint (SchemaCacheErr err)  = hint err
  hint (JwtErr err)          = hint err
  hint NoSchemaCacheError    = Nothing
  hint (PgErr err)           = hint err

instance PgrstError JwtError where
  status JwtDecodeErr{}   = HTTP.unauthorized401
  status JwtSecretMissing = HTTP.status500
  status JwtTokenRequired = HTTP.unauthorized401
  status JwtClaimsErr{}   = HTTP.unauthorized401

  headers e@(JwtDecodeErr _) = [invalidTokenHeader $ message e]
  headers JwtTokenRequired   = [requiredTokenHeader]
  headers e@(JwtClaimsErr _) = [invalidTokenHeader $ message e]
  headers _                  = mempty

instance JSON.ToJSON JwtError where
  toJSON err = toJsonPgrstError
    (code err) (message err) (details err) (hint err)

instance ErrorBody JwtError where
  code JwtSecretMissing = "PGRST300"
  code (JwtDecodeErr _) = "PGRST301"
  code JwtTokenRequired = "PGRST302"
  code (JwtClaimsErr _) = "PGRST303"

  message JwtSecretMissing = "Server lacks JWT secret"
  message (JwtDecodeErr e) = case e of
    EmptyAuthHeader        -> "Empty JWT is sent in Authorization header"
    UnexpectedParts n      -> "Expected 3 parts in JWT; got " <> show n
    KeyError _             -> "No suitable key or wrong key type"
    BadAlgorithm _         -> "Wrong or unsupported encoding algorithm"
    BadCrypto              -> "JWT cryptographic operation failed"
    UnsupportedTokenType   -> "Unsupported token type"
    UnreachableDecodeError -> "JWT couldn't be decoded"
  message JwtTokenRequired = "Anonymous access is disabled"
  message (JwtClaimsErr e) = case e of
    JWTExpired               -> "JWT expired"
    JWTNotYetValid           -> "JWT not yet valid"
    JWTIssuedAtFuture        -> "JWT issued at future"
    JWTNotInAudience         -> "JWT not in audience"
    ParsingClaimsFailed      -> "Parsing claims failed"
    ExpClaimNotNumber        -> "The JWT 'exp' claim must be a number"
    NbfClaimNotNumber        -> "The JWT 'nbf' claim must be a number"
    IatClaimNotNumber        -> "The JWT 'iat' claim must be a number"
    AudClaimNotStringOrArray -> "The JWT 'aud' claim must be a string or an array of strings"

  details (JwtDecodeErr jde) = case jde of
    KeyError dets     -> Just $ JSON.String dets
    BadAlgorithm dets -> Just $ JSON.String dets
    _                 -> Nothing
  details _ = Nothing

  hint _    = Nothing

invalidTokenHeader :: Text -> Header
invalidTokenHeader m =
  ("WWW-Authenticate", "Bearer error=\"invalid_token\", " <> "error_description=" <> encodeUtf8 (show m))

requiredTokenHeader :: Header
requiredTokenHeader = ("WWW-Authenticate", "Bearer")

-- For parsing byteString to JSON Object, used for allowing full response control
data PgRaiseErrMessage = PgRaiseErrMessage {
  getCode    :: Text,
  getMessage :: Text,
  getDetails :: Maybe Text,
  getHint    :: Maybe Text
}

data PgRaiseErrDetails = PgRaiseErrDetails {
  getStatus     :: Int,
  getStatusText :: Maybe Text,
  getHeaders    :: Map Text Text
}

instance JSON.FromJSON PgRaiseErrMessage where
  parseJSON (JSON.Object m) =
    PgRaiseErrMessage
      <$> m .: "code"
      <*> m .: "message"
      <*> m .:? "details"
      <*> m .:? "hint"

  parseJSON _ = mzero

instance JSON.FromJSON PgRaiseErrDetails where
  parseJSON (JSON.Object d) =
    PgRaiseErrDetails
      <$> d .: "status"
      <*> d .:? "status_text"
      <*> d .: "headers"

  parseJSON _ = mzero

parseRaisePGRST :: ByteString -> Maybe ByteString -> Either ApiRequestError (PgRaiseErrMessage, PgRaiseErrDetails)
parseRaisePGRST m d = do
  msgJson <- maybeToRight (PGRSTParseError $ MsgParseError m) (JSON.decodeStrict m)
  det <- maybeToRight (PGRSTParseError NoDetail) d
  detJson <- maybeToRight (PGRSTParseError $ DetParseError det) (JSON.decodeStrict det)
  return (msgJson, detJson)
