module PostgREST.Headers
  ( GucHeader
  , unwrapGucHeader
  , addHeadersIfNotIncluded
  ) where

import qualified Data.Aeson           as JSON
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as M

import Network.HTTP.Types.Header (Header)

import Protolude      hiding (toS)
import Protolude.Conv (toS)


{-|
  Custom guc header, it's obtained by parsing the json in a:
  `SET LOCAL "response.headers" = '[{"Set-Cookie": ".."}]'
-}
newtype GucHeader = GucHeader (CI.CI ByteString, ByteString)

instance JSON.FromJSON GucHeader where
  parseJSON (JSON.Object o) = case headMay (M.toList o) of
    Just (k, JSON.String s) | M.size o == 1 -> pure $ GucHeader (CI.mk $ toS k, toS s)
                            | otherwise     -> mzero
    _ -> mzero
  parseJSON _          = mzero

unwrapGucHeader :: GucHeader -> Header
unwrapGucHeader (GucHeader (k, v)) = (k, v)

-- | Add headers not already included to allow the user to override them instead of duplicating them
addHeadersIfNotIncluded :: [Header] -> [Header] -> [Header]
addHeadersIfNotIncluded newHeaders initialHeaders =
  filter (\(nk, _) -> isNothing $ find (\(ik, _) -> ik == nk) initialHeaders) newHeaders ++
  initialHeaders
