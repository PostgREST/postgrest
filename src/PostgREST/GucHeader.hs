module PostgREST.GucHeader
  ( GucHeader
  , unwrapGucHeader
  , addHeadersIfNotIncluded
  ) where

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Key       as K
import qualified Data.Aeson.KeyMap    as KM
import qualified Data.CaseInsensitive as CI

import Network.HTTP.Types.Header (Header)

import Protolude


{-|
  Custom guc header, it's obtained by parsing the json in a:
  `SET LOCAL "response.headers" = '[{"Set-Cookie": ".."}]'
-}
newtype GucHeader = GucHeader (CI.CI ByteString, ByteString)

instance JSON.FromJSON GucHeader where
  parseJSON (JSON.Object o) =
    case KM.toList o of
      [(k, JSON.String s)] -> pure $ GucHeader (CI.mk $ toUtf8 $ K.toText k, toUtf8 s)
      _ -> mzero
  parseJSON _ = mzero

unwrapGucHeader :: GucHeader -> Header
unwrapGucHeader (GucHeader (k, v)) = (k, v)

-- | Add headers not already included to allow the user to override them instead of duplicating them
addHeadersIfNotIncluded :: [Header] -> [Header] -> [Header]
addHeadersIfNotIncluded newHeaders initialHeaders =
  filter (\(nk, _) -> isNothing $ find (\(ik, _) -> ik == nk) initialHeaders) newHeaders ++
  initialHeaders
