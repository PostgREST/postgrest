{-|
Module      : PostgREST.Auth.JwtCache
Description : PostgREST JWT validation results Cache.

This module provides functions to deal with the JWT cache.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE StrictData                #-}

module PostgREST.Auth.JwtCache
  ( init
  , update
  , JwtCacheState
  , lookupJwtCache
  ) where

import           Control.Concurrent.STM      (newTVarIO, readTVar,
                                              writeTVar)
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad.Error.Class   (liftEither)
import qualified Data.Aeson                  as JSON
import qualified Data.Aeson.KeyMap           as KM
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Internal    as BS
import           Data.Either.Extra
import           Data.IORef                  (IORef, newIORef,
                                              readIORef, writeIORef)
import           Jose.Jwk                    (JwkSet)
import qualified Jose.Jwk                    as JWT
import qualified Jose.Jwt                    as JWT
import           PostgREST.Cache.Sieve       (alwaysValid)
import qualified PostgREST.Cache.Sieve       as SC
import           PostgREST.Config            (AppConfig (..))
import           PostgREST.Error             (Error (..),
                                              JwtClaimsError (..),
                                              JwtDecodeError (..),
                                              JwtError (..))
import           PostgREST.Observation       (Observation (JwtCacheEviction, JwtCacheLookup),
                                              ObservationHandler)

import Protolude

data JwtCacheState = JwtCacheState ObservationHandler (IORef JwtCache)

class CacheVariant m v where
  cached :: (MonadError Error n, MonadIO n) => SC.Cache m ByteString v -> ByteString -> n JSON.Object

{-|
Jwt caching can have three different configurations:
* missing JWT Key (no caching and throw error when JWT token present in the request)
* JWT cache turned off
* JWT cache turned on

All three options are represented by JwtCache data type.

Handling of reconfiguration is centralized in this module.
-}
data JwtCache =
  JwtNoJwks |
  JwtNoCache JwkSet |
  forall m v. CacheVariant m v => JwtCache JwkSet (TVar Int) (SC.Cache m ByteString v)

instance CacheVariant IO (Either Error JSON.Object) where
  cached c = liftIO . SC.cached c >=> liftEither

instance CacheVariant (ExceptT Error IO) JSON.Object where
  cached c = liftIO . runExceptT . SC.cached c >=> liftEither

decode :: (MonadError Error m, MonadIO m) => JwtCache -> ByteString -> m JSON.Object
decode JwtNoJwks        = const $ throwError (JwtErr JwtSecretMissing)
decode (JwtNoCache key) = parseAndDecodeClaims key
decode (JwtCache _ _ c) = cached c

-- | Reconfigure JWT caching and update JwtCacheState accordingly
update :: JwtCacheState -> AppConfig -> IO ()
update (JwtCacheState observationHandler jwtCacheState) config@AppConfig{configJWKS, configJwtCacheMaxEntries} =
  let reinitialize =
        newJwtCache config observationHandler
          >>= writeIORef jwtCacheState
  in
  readIORef jwtCacheState >>= \case
    (JwtCache decodingKey maxSize _) ->
      if configJWKS /= Just decodingKey || configJwtCacheMaxEntries <= 0 then
        -- reinitialize if key changed or cache disabled
        reinitialize
      else
        -- max size changed - set it and let the cache shrink itself if necessary
        atomically $ writeTVar maxSize configJwtCacheMaxEntries

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
          JwtCache key maxSizeTVar <$>
            notCachingErrors (readTVar maxSizeTVar) key

    notCachingErrors :: STM Int -> JwkSet -> IO (SC.Cache (ExceptT Error IO) ByteString JSON.Object)
    notCachingErrors maxSize key = SC.cacheIO (SC.CacheConfig maxSize
            (parseAndDecodeClaims key)
            (lift . observationHandler . JwtCacheLookup) -- lookup metrics
            (const . const $ lift $ observationHandler JwtCacheEviction) -- evictions metrics
            alwaysValid) -- no invalidation for now

lookupJwtCache :: (MonadError Error m, MonadIO m) => JwtCacheState -> Maybe ByteString -> m JSON.Object
lookupJwtCache (JwtCacheState _ cacheState) k = liftIO (readIORef cacheState) >>= flip (maybe (pure KM.empty)) k . decode

parseAndDecodeClaims :: (MonadError Error m, MonadIO m) => JwkSet -> ByteString -> m JSON.Object
parseAndDecodeClaims jwkSet = decodeClaims <=< parseToken jwkSet
  where
    decodeClaims (JWT.Jws (_, claims)) = maybe (throwError (JwtErr $ JwtClaimsErr ParsingClaimsFailed)) pure (JSON.decodeStrict claims)
    decodeClaims _ = throwError $ JwtErr $ JwtDecodeErr UnsupportedTokenType

-- | Receives the JWT secret and audience (from config) and a JWT and returns a
-- JSON object of JWT claims.
parseToken :: (MonadError Error m, MonadIO m) => JwkSet -> ByteString -> m JWT.JwtContent
parseToken _ "" = throwError $ JwtErr $ JwtDecodeErr EmptyAuthHeader
parseToken secret tkn = do
  tknWith3Parts <- hasThreeParts tkn
  eitherContent <- liftIO $ JWT.decode (JWT.keys secret) Nothing tknWith3Parts
  liftEither . mapLeft (JwtErr . jwtDecodeError) $ eitherContent
  where
      hasThreeParts token = case BS.count (BS.c2w '.') token of
        2 -> pure token
        n -> throwError $ JwtErr $ JwtDecodeErr $ UnexpectedParts (n + 1)

      jwtDecodeError :: JWT.JwtError -> JwtError
      -- The only errors we can get from JWT.decode function are:
      --   BadAlgorithm
      --   KeyError
      --   BadCrypto
      jwtDecodeError (JWT.KeyError m)     = JwtDecodeErr $ KeyError m
      jwtDecodeError (JWT.BadAlgorithm m) = JwtDecodeErr $ BadAlgorithm m
      jwtDecodeError JWT.BadCrypto        = JwtDecodeErr BadCrypto
      -- Control never reaches here, the decode function only returns the above three
      jwtDecodeError _                    = JwtDecodeErr UnreachableDecodeError
