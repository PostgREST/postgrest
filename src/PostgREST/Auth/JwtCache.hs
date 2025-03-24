{-|
Module      : PostgREST.Auth.JwtCache
Description : PostgREST Jwt Authentication Result Cache.

This module provides functions to deal with the JWT cache
-}
{-# LANGUAGE CPP #-}
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
#ifdef JWT_CACHE_METRIC /* Include this in a non-profiled postgrest build */
import GHC.DataSize (recursiveSizeNF)
#endif
import System.Clock (TimeSpec (..))

import PostgREST.Auth.Types  (AuthResult (..))
import PostgREST.Error       (Error (..))
import PostgREST.Observation (Observation (..), ObservationHandler)

import Control.Debounce
import Protolude

-- Jwt Cache State
--
-- Calculating the size of each cache entry is an expensive operation. We don't
-- want to recalculate the size of each entry after the cache eviction/purging.
--
-- To avoid this, we store the size of each cache entry with the value of the
-- cache entry as a tuple (AuthResult,SizeInBytes). Now after the purging
-- operation, the size of cache entry will be evicted along with the entry and
-- updating the cache size becomes a simple sum of all sizes that are store in
-- the cache
data JwtCacheState = JwtCacheState
  -- | Jwt Cache
  { jwtCache                     :: C.Cache ByteString (AuthResult,SizeInBytes)
  -- | Calculate cache size with debounce
  , cacheSizeCalcDebounceTimeout :: MVar (IO ())
  }

type SizeInBytes = Int

-- | Initialize JwtCacheState
init :: IO JwtCacheState
init = do
  cache <- C.newCache Nothing -- no default expiration
  JwtCacheState cache <$> newEmptyMVar

-- | Used to retrieve and insert JWT to JWT Cache
lookupJwtCache :: JwtCacheState -> ByteString -> Int -> IO (Either Error AuthResult) -> UTCTime -> ObservationHandler -> IO (Either Error AuthResult)
lookupJwtCache jwtCacheState token maxLifetime parseJwt utc observer = do
  checkCache <- C.lookup (jwtCache jwtCacheState) token
  authResult <- maybe parseJwt (pure . Right . fst) checkCache

  case (authResult, checkCache) of
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

      -- get expiration time
      let timeSpec = getTimeSpec res maxLifetime utc

      -- purge expired cache entries
      C.purgeExpired (jwtCache jwtCacheState)

      -- calculate size of the cache entry to store it with authResult
      sz <- calcCacheEntrySizeInBytes (token,res,timeSpec)

      -- insert new cache entry with byte size
      C.insert' (jwtCache jwtCacheState) (Just timeSpec) token (res,sz)

      -- calculate complete cache size with debounce and log it
      updateCacheSizeWithDebounce jwtCacheState observer

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

-- | Update JwtCacheSize Metric
--
-- Runs the cache size calculation with debounce
updateCacheSizeWithDebounce :: JwtCacheState -> ObservationHandler -> IO ()
updateCacheSizeWithDebounce jwtCacheState observer = do
  cSizeDebouncer <- tryReadMVar $ cacheSizeCalcDebounceTimeout jwtCacheState
  case cSizeDebouncer of
    Just d -> d
    Nothing -> do
      newDebouncer <-
        mkDebounce defaultDebounceSettings
          -- debounceFreq is set to default 1 second
          { debounceAction = calculateSizeThenLog
          , debounceEdge = leadingEdge -- logs at the start and the end
          }
      putMVar (cacheSizeCalcDebounceTimeout jwtCacheState) newDebouncer
      newDebouncer
    where
      calculateSizeThenLog :: IO ()
      calculateSizeThenLog = do
        entries <- C.toList $ jwtCache jwtCacheState
        -- extract the size from each entry and sum them all
        let size = sum [ sz | (_,(_,sz),_) <- entries]
        observer $ JwtCache size -- updates and logs the metric

-- | Calculate JWT Cache Size in Bytes
--
-- The cache size is updated by calculating the size of every
-- cache entry and updating the metric.
--
-- The cache entry consists of
--   key          :: ByteString
--   value        :: AuthReult
--   expire value :: TimeSpec
--
-- We calculate the size of each cache entry component
-- by using recursiveSizeNF function which first evaluates
-- the data structure to Normal Form and then calculate size.
-- The normal form evaluation is necessary for accurate size
-- calculation because haskell is lazy and we dont wanna count
-- the size of large thunks (unevaluated expressions)
calcCacheEntrySizeInBytes :: (ByteString, AuthResult, TimeSpec) -> IO Int
#ifdef JWT_CACHE_METRIC /* Include this in a non-profiled postgrest build */
calcCacheEntrySizeInBytes entry = fromIntegral <$> getSize entry
    where
      getSize :: (ByteString, AuthResult, TimeSpec) -> IO Word
      getSize (bs, ar, ts) = do
        keySize      <- recursiveSizeNF bs
        arClaimsSize <- recursiveSizeNF $ authClaims ar
        arRoleSize   <- recursiveSizeNF $ authRole ar
        timeSpecSize <- liftA2 (+) (recursiveSizeNF (sec ts)) (recursiveSizeNF (nsec ts))
        let sizeOfSizeEntryItself = 8 -- a constant 8 bytes size of each size entry in the cache
        return (keySize + arClaimsSize + arRoleSize + timeSpecSize + sizeOfSizeEntryItself)
#else /* otherwise set it to 0 for a profiled build (used in memory-tests) */
calcCacheEntrySizeInBytes _ = return 0
#endif
