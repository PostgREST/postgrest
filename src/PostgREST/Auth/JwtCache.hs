{-|
Module      : PostgREST.Auth.JwtCache
Description : PostgREST Jwt Authentication Result Cache.

This module provides functions to deal with the JWT cache
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE StrictData                #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module PostgREST.Auth.JwtCache
  ( init
  , update
  , JwtCacheState
  , lookupJwtCache
  ) where

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.KeyMap as KM

import PostgREST.Error (Error (..), JwtError (JwtSecretMissing))

import           Control.Concurrent.STM      (newTVarIO, writeTVar)
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad.Error.Class   (liftEither)
import           Data.ByteString             hiding (init)
import           Data.IORef                  (IORef, newIORef,
                                              readIORef, writeIORef)
import           Jose.Jwk                    (JwkSet)
import           PostgREST.Auth.Jwt          (parseAndDecodeClaims)
import qualified PostgREST.Cache.Sieve       as SC
import           PostgREST.Config            (AppConfig (..))
import           PostgREST.Observation       (Observation (JwtCacheEviction, JwtCacheLookup),
                                              ObservationHandler)
import           Protolude

data JwtCacheState = JwtCacheState ObservationHandler (IORef JwtCache)

class CacheVariant m v where
  cached :: SC.Cache m ByteString v -> ByteString -> ExceptT Error IO JSON.Object

data JwtCache =
  JwtNoJwks |
  JwtNoCache JwkSet |
  forall m v. CacheVariant m v => JwtCache JwkSet (TVar Int) (SC.Cache m ByteString v)

instance CacheVariant IO (Either Error JSON.Object) where
  cached c = lift . SC.cached c >=> liftEither

instance CacheVariant (ExceptT Error IO) JSON.Object where
  cached = SC.cached

decode :: JwtCache -> ByteString -> ExceptT Error IO JSON.Object
decode JwtNoJwks        = const $ throwError (JwtErr JwtSecretMissing)
decode (JwtNoCache key) = parseAndDecodeClaims key
decode (JwtCache _ _ c) = cached c

update :: JwtCacheState -> AppConfig -> IO ()
update (JwtCacheState observationHandler jwtCacheState) config@AppConfig{configJWKS, configJwtCacheMaxSize} =
  readIORef jwtCacheState >>= \case
    (JwtCache decodingKey maxSize _) ->
      if configJWKS /= Just decodingKey || configJwtCacheMaxSize <= 0 then
        -- key changed or cache disabled - reinit
        newJwtCache config observationHandler >>= writeIORef jwtCacheState
      else
        -- leave cache but set new maxSize
        -- the cache is going to resize itself
        atomically $ writeTVar maxSize configJwtCacheMaxSize

    _ -> newJwtCache config observationHandler >>= writeIORef jwtCacheState

init :: AppConfig -> ObservationHandler -> IO JwtCacheState
init config handler = (newJwtCache config handler >>= newIORef) <&> JwtCacheState handler

-- | Initialize JwtCacheState
newJwtCache :: AppConfig -> ObservationHandler -> IO JwtCache
newJwtCache AppConfig{configJWKS, configJwtCacheMaxSize} observationHandler = do
  maybe (pure JwtNoJwks) initCache configJWKS
  where
    initCache key =
      if configJwtCacheMaxSize > 0 then do
        maxSize <- newTVarIO configJwtCacheMaxSize
        JwtCache key maxSize <$>
          -- select cachingErrors or notCachingErrors
          cachingErrors maxSize key
      else
        pure $ JwtNoCache key

    cachingErrors maxSize key = SC.cacheIO maxSize
            (runExceptT . parseAndDecodeClaims key)
            (observationHandler . JwtCacheLookup) -- lookup metrics
            (observationHandler JwtCacheEviction) -- evictions metrics

    notCachingErrors maxSize key = SC.cacheIO maxSize
            (parseAndDecodeClaims key)
            (lift . observationHandler . JwtCacheLookup) -- lookup metrics
            (lift $ observationHandler JwtCacheEviction) -- evictions metrics

lookupJwtCache :: JwtCacheState -> Maybe ByteString -> ExceptT Error IO JSON.Object
lookupJwtCache (JwtCacheState _ cacheState) k = liftIO (readIORef cacheState) >>= flip (maybe (pure KM.empty)) k . decode
