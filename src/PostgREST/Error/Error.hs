{-# OPTIONS_GHC -fno-warn-orphans #-}

module PostgREST.Error.Error
  ( Error (..)
  , JwtError (..)
  , JwtDecodeError (..)
  , JwtClaimsError (..)
  ) where

import qualified Data.Aeson                  as JSON
import qualified Data.Text.Encoding          as T
import qualified Network.HTTP.Types          as HTTP

import PostgREST.Error.Algebra
import PostgREST.Error.ApiRequestError
import PostgREST.Error.PgError
import PostgREST.Error.SchemaCacheError
import Protolude

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

invalidTokenHeader :: Text -> HTTP.Header
invalidTokenHeader m =
  ("WWW-Authenticate", "Bearer error=\"invalid_token\", " <> "error_description=" <> encodeUtf8 (show m))

requiredTokenHeader :: HTTP.Header
requiredTokenHeader = ("WWW-Authenticate", "Bearer")

proxyStatusHeader :: Text -> HTTP.Header
proxyStatusHeader code' = ("Proxy-Status", "PostgREST; error=" <> T.encodeUtf8 code')
