{-|
Module      : PostgREST.Auth.JwtCache
Description : PostgREST Jwt Authentication Result Cache.

This module provides functions to deal with the JWT cache
-}
module PostgREST.Auth.JwtCache
  ( init
  , JwtCacheState
  , lookupJwtCache
  ) where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Cache.LRU    as C
import qualified Data.IORef        as I
import qualified Data.Scientific   as Sci

import Data.Time.Clock       (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Num               (integerFromInt)

import PostgREST.Auth.Types (AuthResult (..))
import PostgREST.Error      (Error (..))

import Protolude

-- | Jwt Cache State
newtype JwtCacheState = JwtCacheState
  { maybeJwtCache :: Maybe (I.IORef (C.LRU ByteString AuthResult))
  }

-- | Initialize JwtCacheState
init :: Int -> IO JwtCacheState
init 0          = return $ JwtCacheState Nothing
init maxEntries = do
  cache <- I.newIORef $ C.newLRU (Just $ integerFromInt maxEntries)
  return $ JwtCacheState $ Just cache


-- | Used to retrieve and insert JWT to JWT Cache
lookupJwtCache :: JwtCacheState -> ByteString -> IO (Either Error AuthResult) -> UTCTime -> IO (Either Error AuthResult)
lookupJwtCache jwtCacheState token parseJwt utc = do
  case maybeJwtCache jwtCacheState of
    Nothing            -> parseJwt
    Just jwtCacheIORef -> do
      -- get cache from IORef
      jwtCache <- I.readIORef jwtCacheIORef

      -- MAKE SURE WE UPDATE THE CACHE ON ALL PATHS AFTER LOOKUP
      -- This is because it is a pure LRU cache, so lookup returns the
      -- the cache with new state, hence it should be updated
      let (jwtCache', maybeVal) = C.lookup token jwtCache

      case maybeVal of
        Nothing -> do -- CACHE MISS

            -- When we get a cache miss, we get the parse result, insert it
            -- into the cache. After that, we write the cache IO ref with
            -- updated cache
            authResult <- parseJwt

            case authResult of
              Right result -> do
                  -- insert token -> update cache -> return token
                  let jwtCache'' = C.insert token result jwtCache'
                  I.writeIORef jwtCacheIORef jwtCache''
                  return $ Right result
              Left e -> do
                  -- update cache after lookup -> return error
                  I.writeIORef jwtCacheIORef jwtCache'
                  return $ Left e

        Just result -> -- CACHE HIT

            -- For cache hit, we get the result from cache, we check the
            -- exp claim. If it expired, we delete it from cache and parse
            -- the jwt. Otherwise, the hit result is valid, so we return it

            if isExpClaimExpired result utc then do
                -- delete token -> update cache -> parse token
                let (jwtCache'',_) = C.delete token jwtCache'
                I.writeIORef jwtCacheIORef jwtCache''
                parseJwt
            else do
                -- update cache after lookup -> return result
                I.writeIORef jwtCacheIORef jwtCache'
                return $ Right result


type Expired = Bool

-- | Check if exp claim is expired when looked up from cache
isExpClaimExpired :: AuthResult -> UTCTime -> Expired
isExpClaimExpired result utc =
    case expireJSON of
      Nothing                      -> False -- if exp not present then it is valid
      Just (JSON.Number expiredAt) -> (sciToInt expiredAt - now) < 0
      Just _                       -> False -- if exp is not a number then valid
      where
        expireJSON = KM.lookup "exp" (authClaims result)
        now = (floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) utc :: Int
        sciToInt = fromMaybe 0 . Sci.toBoundedInteger
