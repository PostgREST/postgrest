{-|
Module      : PostgREST.Auth.Cache
Description : Cache to store parsed Jwt Authentication Result
-}
module PostgREST.Auth.Cache
  ( getJWTFromCache )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Cache        as C
import qualified Data.Scientific   as Sci

import Data.Maybe            (fromJust)
import Data.Time.Clock       (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Clock          (TimeSpec (..))
import System.IO.Unsafe      (unsafePerformIO)

import PostgREST.AppState    (AppState, getJwtCacheState, getObserver)
import PostgREST.Auth.Types  (AuthResult (..), JwtCacheState (..))
import PostgREST.Error       (Error (..))
import PostgREST.Internal    (recursiveSizeNF)
import PostgREST.Observation (Observation (..))

import Protolude

-- | Used to retrieve and insert JWT to JWT Cache
getJWTFromCache :: AppState -> ByteString -> Int -> IO (Either Error AuthResult) -> UTCTime -> IO (Either Error AuthResult)
getJWTFromCache appState token maxLifetime parseJwt utc = do

  checkCache <- C.lookup jwtCache token
  authResult <- maybe parseJwt (pure . Right) checkCache

  -- if token not found, add to cache and increment cache size metric
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

      -- purge expired cache entries (VERY INEFFICIENT)
      C.purgeExpired jwtCache

      -- insert new cache entry
      C.insert' jwtCache (Just timeSpec) token res

      -- calculate cache size on a new thread
      updateJwtCacheSizeMetric appState

    _                    -> pure ()

  return authResult
    where
      jwtCache = cache $ getJwtCacheState appState

-- | Used to extract JWT exp claim and add to JWT Cache
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
-- Running the cache size calculation on a new thread.
-- To make this thread safe, a semaphore is used. It
-- allows only 1 thread at a time to run the calculation blocking
-- new threads until signaled
updateJwtCacheSizeMetric :: AppState -> IO ()
updateJwtCacheSizeMetric appState = do
  _ <- forkIO $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    size <- calcCacheSizeInBytes jwtCache
    observer $ JWTCache size
  return ()
      where
        sem = (semaphore . getJwtCacheState) appState
        jwtCache = (cache . getJwtCacheState) appState
        observer = getObserver appState

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
calcCacheSizeInBytes :: C.Cache ByteString AuthResult -> IO Int
calcCacheSizeInBytes jwtCache = do
  cacheList <- C.toList jwtCache
  let szList = [ unsafePerformIO (getSize (bs, ar, fromJust ts)) | (bs, ar, ts) <- cacheList]
  return $ fromIntegral (sum szList)
    where
      getSize :: (ByteString, AuthResult, TimeSpec) -> IO Word
      getSize (bs, ar, ts) = do
        keySize      <- recursiveSizeNF bs
        arClaimsSize <- recursiveSizeNF $ authClaims ar
        arRoleSize   <- recursiveSizeNF $ authRole ar
        timeSpecSize <- liftA2 (+) (recursiveSizeNF (sec ts)) (recursiveSizeNF (nsec ts))

        return (keySize + arClaimsSize + arRoleSize + timeSpecSize)
