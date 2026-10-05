{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : PostgREST.Auth.JwtCache
-- Description : PostgREST JWT validation results Cache.
--
-- This module provides functions to deal with the JWT cache.
module PostgREST.Auth.JwtCache
  ( init
  , update
  , JwtCacheState
  , lookupJwtCache
  )
where

import Control.Concurrent.STM (newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Error.Class (liftEither)
import Data.ByteString hiding (all, elem, init)
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Jose.Jwk (Jwk, JwkSet (..))
import Protolude

import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KM

import PostgREST.Auth.Jwt (parseAndDecodeClaims)
import PostgREST.Cache.Sieve (Discard (..))
import PostgREST.Config (AppConfig (..))
import PostgREST.Error (Error (..), JwtError (JwtSecretMissing))
import PostgREST.Observation
  ( Observation (JwtCacheEviction, JwtCacheLookup)
  , ObservationHandler
  )

import PostgREST.Cache.Sieve qualified as SC

data JwtCacheState = JwtCacheState ObservationHandler (IORef JwtCache)

-- |
-- Jwt caching can have three different configurations:
-- * missing JWT Key (no caching and throw error when JWT token present in the request)
-- * JWT cache turned off
-- * JWT cache turned on
--
-- All three options are represented by JwtCache data type.
--
-- Handling of reconfiguration is centralized in this module.
data JwtCache
  = JwtNoJwks
  | JwtNoCache JwkSet
  | JwtCache (TVar JwkSet) (TVar Int) (SC.Cache (ExceptT Error IO) ByteString (JSON.Object, Maybe Jwk))

decode :: (MonadError Error m, MonadIO m) => JwtCache -> ByteString -> m JSON.Object
decode JwtNoJwks = const $ throwError (JwtErr JwtSecretMissing)
decode (JwtNoCache key) = fmap fst . parseAndDecodeClaims key
decode (JwtCache _ _ c) = fmap fst . (liftIO . runExceptT . SC.cached c >=> liftEither)

-- | Reconfigure JWT caching and update JwtCacheState accordingly
update :: JwtCacheState -> AppConfig -> IO ()
update (JwtCacheState observationHandler jwtCacheState) config@AppConfig{configJWKS, configJwtCacheMaxEntries} =
  let reinitialize =
        newJwtCache config observationHandler
          >>= writeIORef jwtCacheState
  in  readIORef jwtCacheState >>= \case
        (JwtCache decodingKey maxSize _) ->
          if isNothing configJWKS || configJwtCacheMaxEntries <= 0 then
            -- reinitialize if key removed or cache disabled
            reinitialize
          else
            -- key or max size changed - set them and let the cache shrink itself if necessary
            -- (cached JWTs verified with a key that is gone are verified again)
            atomically $ traverse_ (writeTVar decodingKey) configJWKS *> writeTVar maxSize configJwtCacheMaxEntries
        _ -> reinitialize

init :: AppConfig -> ObservationHandler -> IO JwtCacheState
init config = fmap (<$>) JwtCacheState <*> (newJwtCache config >=> newIORef)

-- | Initialize JwtCacheState
newJwtCache :: AppConfig -> ObservationHandler -> IO JwtCache
newJwtCache AppConfig{configJWKS, configJwtCacheMaxEntries} observationHandler = do
  maybe (pure JwtNoJwks) initCache configJWKS
  where
    initCache key = if configJwtCacheMaxEntries <= 0 then pure (JwtNoCache key) else createCache key configJwtCacheMaxEntries

    createCache key maxSize = do
      maxSizeTVar <- newTVarIO maxSize
      keyTVar <- newTVarIO key
      JwtCache keyTVar maxSizeTVar
        <$> notCachingErrors (readTVar maxSizeTVar) keyTVar

    notCachingErrors :: STM Int -> TVar JwkSet -> IO (SC.Cache (ExceptT Error IO) ByteString (JSON.Object, Maybe Jwk))
    notCachingErrors maxSize key =
      SC.cacheIO
        ( SC.CacheConfig
            maxSize
            copy
            (\token -> liftIO (readTVarIO key) >>= (`parseAndDecodeClaims` token))
            (lift . observationHandler . JwtCacheLookup) -- lookup metrics
            (const . const $ lift $ observationHandler JwtCacheEviction) -- evictions metrics
            -- a JWT verified with a key that is not a current key anymore is verified again
            (const . liftIO $ (\current (_, verifiedWith) -> pure $ if maybe False (`elem` keys current) verifiedWith then Nothing else Just (Refresh pass)) <$> readTVarIO key)
        )

lookupJwtCache :: (MonadError Error m, MonadIO m) => JwtCacheState -> Maybe ByteString -> m JSON.Object
lookupJwtCache (JwtCacheState _ cacheState) k = liftIO (readIORef cacheState) >>= flip (maybe (pure KM.empty)) k . decode
