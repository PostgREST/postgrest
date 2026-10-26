{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module      : PostgREST.Auth.JwtCache
-- Description : PostgREST JWT validation results Cache.
--
-- This module provides functions to deal with the JWT cache.
module PostgREST.Auth.JwtCache
  ( init
  , update
  , JwtCacheState
  , JwtKey (..)
  , lookupJwtCache
  )
where

import Control.Concurrent.STM (newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Error.Class (liftEither)
import Data.ByteString hiding (all, init)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Hashable (hashByteArrayWithSalt, hashPtrWithSalt)
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Foreign.C.Types (CChar, CInt (..), CSize (..))
import GHC.Exts (ByteArray#)
import Jose.Jwk (JwkSet)
import Protolude

import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Unsafe qualified as BSU
import System.IO.Unsafe qualified as Unsafe

import PostgREST.Auth.Jwt (parseAndDecodeClaims)
import PostgREST.Cache.Sieve (alwaysValid)
import PostgREST.Config (AppConfig (..))
import PostgREST.Error (Error (..), JwtError (JwtSecretMissing))
import PostgREST.Observation
  ( Observation (JwtCacheEviction, JwtCacheLookup)
  , ObservationHandler
  )

import PostgREST.Cache.Sieve qualified as SC

data JwtCacheState = JwtCacheState ObservationHandler (IORef JwtCache)

-- | Key of the JWT cache.
--
-- A lookup key is a slice of the Authorization header, which can keep a much
-- larger pinned HTTP input buffer alive. It is used for lookups only and is
-- never stored: the cache stores an unpinned copy (see 'storedKey').
-- Both forms are equal and hash the same when they hold the same bytes.
data JwtKey
  = Lookup !ByteString
  | Stored !ShortByteString
  deriving (Show)

jwtKeyBytes :: JwtKey -> ByteString
jwtKeyBytes (Lookup bs) = bs
jwtKeyBytes (Stored sbs) = SBS.fromShort sbs

storedKey :: JwtKey -> JwtKey
storedKey (Lookup bs) = Stored (SBS.toShort bs)
storedKey k = k

foreign import ccall unsafe "string.h memcmp"
  c_memcmp :: ByteArray# -> Ptr CChar -> CSize -> IO CInt

-- | Compare without copying. Passing an unpinned ByteArray# to an unsafe call
-- is fine, because GC cannot run during it.
sameBytes :: ByteString -> ShortByteString -> Bool
sameBytes bs sbs@(SBS ba) =
  BS.length bs == SBS.length sbs
    && Unsafe.unsafeDupablePerformIO
      (BSU.unsafeUseAsCStringLen bs $ \(p, n) -> (== 0) <$> c_memcmp ba p (fromIntegral n))

instance Eq JwtKey where
  Lookup a == Lookup b = a == b
  Stored a == Stored b = a == b
  Lookup a == Stored b = sameBytes a b
  Stored a == Lookup b = sameBytes b a

-- Both forms must hash equal bytes to equal values
instance Hashable JwtKey where
  hashWithSalt salt (Lookup bs) =
    Unsafe.unsafeDupablePerformIO $
      BSU.unsafeUseAsCStringLen bs $
        \(p, len) -> hashPtrWithSalt p len (hashWithSalt salt len)
  hashWithSalt salt (Stored sbs@(SBS ba)) =
    let len = SBS.length sbs in hashByteArrayWithSalt ba 0 len (hashWithSalt salt len)

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
  | JwtCache JwkSet (TVar Int) (SC.Cache (ExceptT Error IO) JwtKey JSON.Object)

decode :: (MonadError Error m, MonadIO m) => JwtCache -> ByteString -> m JSON.Object
decode JwtNoJwks = const $ throwError (JwtErr JwtSecretMissing)
decode (JwtNoCache key) = parseAndDecodeClaims key
decode (JwtCache _ _ c) = liftIO . runExceptT . SC.cached c . Lookup >=> liftEither

-- | Reconfigure JWT caching and update JwtCacheState accordingly
update :: JwtCacheState -> AppConfig -> IO ()
update (JwtCacheState observationHandler jwtCacheState) config@AppConfig{configJWKS, configJwtCacheMaxEntries} =
  let reinitialize =
        newJwtCache config observationHandler
          >>= writeIORef jwtCacheState
  in  readIORef jwtCacheState >>= \case
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
      JwtCache key maxSizeTVar
        <$> notCachingErrors (readTVar maxSizeTVar) key

    notCachingErrors :: STM Int -> JwkSet -> IO (SC.Cache (ExceptT Error IO) JwtKey JSON.Object)
    notCachingErrors maxSize key =
      SC.cacheIO
        ( SC.CacheConfig
            maxSize
            storedKey
            (parseAndDecodeClaims key . jwtKeyBytes)
            (lift . observationHandler . JwtCacheLookup) -- lookup metrics
            (const . const $ lift $ observationHandler JwtCacheEviction) -- evictions metrics
            alwaysValid -- no invalidation for now
        )

lookupJwtCache :: (MonadError Error m, MonadIO m) => JwtCacheState -> Maybe ByteString -> m JSON.Object
lookupJwtCache (JwtCacheState _ cacheState) k = liftIO (readIORef cacheState) >>= flip (maybe (pure KM.empty)) k . decode
