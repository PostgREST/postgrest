{-|
Module      : PostgREST.Auth.JwtCache
Description : PostgREST Jwt Authentication Result Cache.

This module provides functions to deal with the JWT cache
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module PostgREST.Auth.JwtCache
  ( init
  , update
  , JwtCacheState
  , lookupJwtCache
  , accessStats
  , evictionsCount
  ) where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM

import PostgREST.Error      (Error (..), JwtError (JwtSecretMissing))

import PostgREST.Config (AppConfig (..))
import qualified PostgREST.Cache.Sieve as SC
import Jose.Jwk (JwkSet)
import PostgREST.Cache.Sieve (cacheIO, AccessStats, accessStatsIO, evictionsCountIO)
import Control.Concurrent.STM ( newTVarIO, writeTVar )
import PostgREST.Auth.Jwt (parseAndDecodeClaims)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Error.Class (liftEither)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.ByteString hiding (init)
import Protolude


type JwtCacheState = IORef JwtCache
-- | JWT Cache and IO action that triggers purging old entries from the cache
data JwtCache = JwtNoCache {
    lookup :: Maybe ByteString ->  ExceptT Error IO JSON.Object,
    accessStats :: IO AccessStats,
    evictionsCount :: IO Int64
  }|
  JwtCache {
    lookup :: Maybe ByteString -> ExceptT Error IO JSON.Object,
    decodingKey :: JwkSet,
    maxSize :: TVar Int,
    cache :: SC.Cache IO ByteString (Either Error JSON.Object),
    accessStats :: IO AccessStats,
    evictionsCount :: IO Int64
  }

update :: AppConfig -> JwtCacheState -> IO ()
update config@AppConfig{configJWKS, configJwtCacheMaxSize} jwtCacheState = do
  readIORef jwtCacheState >>= \case
    JwtNoCache{} -> newJwtCache config >>= writeIORef jwtCacheState
    JwtCache{..} ->
      if configJWKS /= Just decodingKey then do
        -- key changed - reinit
        newJwtCache config >>= writeIORef jwtCacheState
      else
        -- leave cache but set new maxSize
        -- the cache is going to resize itself
        atomically $ writeTVar maxSize configJwtCacheMaxSize

init :: AppConfig -> IO JwtCacheState
init = newJwtCache >=> newIORef

-- | Initialize JwtCacheState
newJwtCache :: AppConfig -> IO JwtCache
newJwtCache AppConfig{configJWKS, configJwtCacheMaxSize} = do
  maybe (noCache missingSecrets) initCache configJWKS
  where
    noTokenOr = maybe $ pure KM.empty
    missingSecrets = const (throwError $ JwtErr JwtSecretMissing)
    noCache parse = pure $ JwtNoCache (noTokenOr parse) (pure mempty) (pure 0)
    initCache key = if configJwtCacheMaxSize > 0 then do
        maxSize <- newTVarIO configJwtCacheMaxSize
        c <- cacheIO maxSize (runExceptT . parseAndDecodeClaims key)
        let lookupCached = lift . SC.cached c >=> liftEither
        JwtCache
          (noTokenOr lookupCached)
          key maxSize <$>
          cacheIO maxSize (runExceptT . parseAndDecodeClaims key) <*>
          pure (accessStatsIO c) <*>
          pure (evictionsCountIO c)
      else
        noCache $ parseAndDecodeClaims key

lookupJwtCache :: JwtCacheState -> Maybe ByteString -> ExceptT Error IO JSON.Object
lookupJwtCache cacheState k = liftIO (readIORef cacheState) >>= flip lookup k
