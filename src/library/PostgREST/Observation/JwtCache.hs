{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : PostgREST.Observation.JwtCache
-- Description : Observations of the JWT cache.
module PostgREST.Observation.JwtCache
  ( JwtCacheObservation (..)
  , jwtCacheMessage
  )
where

import Protolude

data JwtCacheObservation
  = JwtCacheLookup Bool
  | JwtCacheEviction

jwtCacheMessage :: JwtCacheObservation -> Text
jwtCacheMessage = \case
  JwtCacheLookup _ -> "Looked up a JWT in JWT cache"
  JwtCacheEviction -> "Evicted entry from JWT cache"
