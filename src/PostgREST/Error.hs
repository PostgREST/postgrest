{-|
Module      : PostgREST.Error
Description : PostgREST error HTTP responses
-}

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

import PostgREST.Error.Algebra
import PostgREST.Error.ApiRequestError
import PostgREST.Error.CommandError ()
import PostgREST.Error.PgError
import PostgREST.Error.Error
import PostgREST.Error.ResultError ()
import PostgREST.Error.SchemaCacheError
import PostgREST.Error.UsageError ()
