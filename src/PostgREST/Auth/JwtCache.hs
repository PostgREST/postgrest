{-|
Module      : PostgREST.Auth.Caching
Description : PostgREST JWT validation results Cache.

This module provides functions to deal with the JWT cache.
-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module PostgREST.Auth.JwtCache
  ( init
  , update
  , JwtCacheState
  , lookupJwtCache
  ) where

import qualified Data.Aeson as JSON

import PostgREST.Error (Error (..), JwtError (JwtSecretMissing))

import           Control.Concurrent.STM      (newTVarIO, readTVar,
                                              writeTVar)
import           Control.Concurrent.STM.TVar (TVar)
import           Data.ByteString             hiding (all, init)
import           Data.IORef                  (IORef, newIORef,
                                              readIORef, writeIORef)
import           Data.Time.Clock             (UTCTime)
import           Jose.Jwk                    (JwkSet)
import           PostgREST.Auth.Jwt          (Validated (getValidated),
                                              Validation (..),
                                              parseAndDecodeClaims,
                                              validateAud,
                                              validateTimeClaims,
                                              (>>>))
import           PostgREST.Cache.Sieve       (alwaysValid)
import qualified PostgREST.Cache.Sieve       as SC
import           PostgREST.Config            (AppConfig (..),
                                              audMatchesCfg)
import           PostgREST.Observation       (Observation (JwtCacheEviction, JwtCacheLookup),
                                              ObservationHandler)
import           Protolude

type Cache m v = SC.Cache m ByteString v

type SelectedCacheVariant = Cache (ExceptT Error IO) (Validated '[Aud] JSON.Object)

data CacheState =
  JwksNotConfigured |
  NotCaching JwkSet (Text -> Bool) |
  Caching JwkSet AppConfig (TVar Int) SelectedCacheVariant

data JwtCacheState = JwtCacheState ObservationHandler (IORef CacheState)

parseAndValidateAud :: CacheState -> ByteString -> ExceptT Error IO (Validated '[Aud] JSON.Object)
parseAndValidateAud (Caching _ config _ c) = lookup config c
parseAndValidateAud (NotCaching key audMatches) = parseAndDecodeClaims key >=> validateAud audMatches
parseAndValidateAud JwksNotConfigured = const $ throwError (JwtErr JwtSecretMissing)

class NeedsReinitialize v where
  needsReinitialize :: AppConfig -> AppConfig -> Bool

instance NeedsReinitialize JSON.Object where
  needsReinitialize _ _ = False

instance NeedsReinitialize (Validated (Aud : rest) JSON.Object) where
  needsReinitialize old new = configJwtAudience old /= configJwtAudience new

instance NeedsReinitialize v => NeedsReinitialize (SC.Cache m k v) where
  needsReinitialize = needsReinitialize @v

class CacheVariant c where
  newCache :: STM Int -> JwkSet -> ObservationHandler -> AppConfig -> IO c
  lookup :: AppConfig -> c -> ByteString -> ExceptT Error IO (Validated '[Aud] JSON.Object)

-- Cache parsed JWTs with valid signature and valid aud
instance CacheVariant (Cache (ExceptT Error IO) (Validated '[Aud] JSON.Object)) where
  newCache maxSize key observationHandler config = SC.cacheIO (SC.CacheConfig maxSize
            (parseAndDecodeClaims key >=> validateAud (audMatchesCfg config))
            (lift . observationHandler . JwtCacheLookup) -- lookup metrics
            (const . const $ lift $ observationHandler JwtCacheEviction) -- evictions metrics
            alwaysValid) -- no invalidation for now
  lookup _ = SC.cached

-- | Reconfigure JWT caching and update JwtCacheState accordingly
update :: JwtCacheState -> AppConfig -> IO ()
update (JwtCacheState observationHandler ref) config@AppConfig{configJWKS, configJwtCacheMaxEntries} =
  readIORef ref >>= \case
    (Caching decodingKey oldConfig maxSize cache) ->
      if configJWKS /= Just decodingKey ||
        configJwtCacheMaxEntries <= 0 ||
        needsReinitialize @SelectedCacheVariant oldConfig config
      then
        -- reinitialize if key changed or cache disabled or the cache requires reinitialization
        reinitialize
      else do
        -- max size changed - set it and let the cache resize itself if necessary
        atomically $ writeTVar maxSize configJwtCacheMaxEntries
        -- save new config for future updates
        writeIORef ref $ Caching decodingKey config maxSize cache
    _ -> reinitialize
  where
    reinitialize = newJwtCacheState config observationHandler >>= writeIORef ref

init :: AppConfig -> ObservationHandler -> IO JwtCacheState
init config = fmap (<$>) JwtCacheState <*> (newJwtCacheState config >=> newIORef)

-- | Initialize JwtCacheState
newJwtCacheState :: AppConfig -> ObservationHandler -> IO CacheState
newJwtCacheState config@AppConfig{configJWKS, configJwtCacheMaxEntries} observationHandler =
  maybe (pure JwksNotConfigured) initCache configJWKS
  where
    initCache key =
      if configJwtCacheMaxEntries <= 0 then
        pure $ NotCaching key (audMatchesCfg config)
      else
        createCache key configJwtCacheMaxEntries

    createCache key maxSize = do
      maxSizeTVar <- newTVarIO maxSize
      Caching key config maxSizeTVar <$>
        newCache
          (readTVar maxSizeTVar) key observationHandler config

parseAndValidateClaims :: UTCTime -> ByteString -> CacheState -> ExceptT Error IO (Validated [Aud, Time] JSON.Object)
parseAndValidateClaims time k c = (parseAndValidateAud c >>> validateTimeClaims time) k

lookupJwtCache :: JwtCacheState -> UTCTime -> ByteString -> ExceptT Error IO JSON.Object
lookupJwtCache (JwtCacheState _ cacheState) time k =
  liftIO (readIORef cacheState) >>= fmap getValidated . parseAndValidateClaims time k
