{-|
Module      : PostgREST.Auth.JwtCache
Description : PostgREST Jwt Authentication Result Cache.

This module provides functions to deal with the JWT cache
-}
{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.Auth.JwtCache
  ( init
  , JwtCacheState
  , lookupJwtCache
  ) where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Cache        as C
import qualified Data.Scientific   as Sci

import Data.Time.Clock       (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Clock          (TimeSpec (..))

import PostgREST.Auth.Types (AuthResult (..))
import PostgREST.Error      (Error (..))

import Protolude

newtype JwtCacheState = JwtCacheState
  { jwtCache :: C.Cache ByteString AuthResult
  }

-- | Initialize JwtCacheState
init :: IO JwtCacheState
init = do
  cache <- C.newCache Nothing -- no default expiration
  return $ JwtCacheState cache

-- | Used to retrieve and insert JWT to JWT Cache
lookupJwtCache :: JwtCacheState -> ByteString -> Int -> IO (Either Error AuthResult) -> UTCTime -> IO (Either Error AuthResult)
lookupJwtCache JwtCacheState{jwtCache} token maxLifetime parseJwt utc = do
  checkCache <- C.lookup jwtCache token
  authResult <- maybe parseJwt (pure . Right) checkCache

  case (authResult,checkCache) of
    -- From comment:
    -- https://github.com/PostgREST/postgrest/pull/3801#discussion_r1857987914
    --
    -- We purge expired cache entries on a cache miss
    -- The reasoning is that:
    --
    -- 1. We expect it to be rare (otherwise there is no point of the cache)
    -- 2. It makes sure the cache is not growing (as inserting new entries
    --    does garbage collection)
    -- 3. Since this is time expiration based cache there is no real risk of
    --    starvation - sooner or later we are going to have a cache miss.

    (Right res, Nothing) -> do -- cache miss

      let timeSpec = getTimeSpec res maxLifetime utc

      -- purge expired cache entries
      C.purgeExpired jwtCache

      -- insert new cache entry
      C.insert' jwtCache (Just timeSpec) token res

    _                    -> pure ()

  return authResult

-- Used to extract JWT exp claim and add to JWT Cache
getTimeSpec :: AuthResult -> Int -> UTCTime -> TimeSpec
getTimeSpec res maxLifetime utc = do
  let expireJSON = KM.lookup "exp" (authClaims res)
      utcToSecs = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
      sciToInt = fromMaybe 0 . Sci.toBoundedInteger
  case expireJSON of
    Just (JSON.Number seconds) -> TimeSpec (sciToInt seconds - utcToSecs utc) 0
    _                          -> TimeSpec (fromIntegral maxLifetime :: Int64) 0
