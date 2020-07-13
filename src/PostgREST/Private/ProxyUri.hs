
{-|
Module      : PostgREST.Private.ProxyUri
Description : Proxy Uri validator
-}
module PostgREST.Private.ProxyUri (
  isMalformedProxyUri
, toURI
) where

import Data.Maybe  (fromJust)
import Data.Text   (pack, toLower)
import Network.URI (URI (..), URIAuth (..), isAbsoluteURI, parseURI)

import Protolude      hiding (Proxy, dropWhile, get, intercalate,
                       toLower, toS, (&))
import Protolude.Conv (toS)

{-|
  Test whether a proxy uri is malformed or not.
  A valid proxy uri should be an absolute uri without query and user info,
  only http(s) schemes are valid, port number range is 1-65535.

  For example
  http://postgrest.com/openapi.json
  https://postgrest.com:8080/openapi.json
-}
isMalformedProxyUri :: Maybe Text -> Bool
isMalformedProxyUri Nothing =  False
isMalformedProxyUri (Just uri)
  | isAbsoluteURI (toS uri) = not $ isUriValid $ toURI uri
  | otherwise = True

toURI :: Text -> URI
toURI uri = fromJust $ parseURI (toS uri)

isUriValid:: URI -> Bool
isUriValid = fAnd [isSchemeValid, isQueryValid, isAuthorityValid]

fAnd :: [a -> Bool] -> a -> Bool
fAnd fs x = all ($ x) fs

isSchemeValid :: URI -> Bool
isSchemeValid URI {uriScheme = s}
  | toLower (pack s) == "https:" = True
  | toLower (pack s) == "http:" = True
  | otherwise = False

isQueryValid :: URI -> Bool
isQueryValid URI {uriQuery = ""} = True
isQueryValid _                   = False

isAuthorityValid :: URI -> Bool
isAuthorityValid URI {uriAuthority = a}
  | isJust a = fAnd [isUserInfoValid, isHostValid, isPortValid] $ fromJust a
  | otherwise = False

isUserInfoValid :: URIAuth -> Bool
isUserInfoValid URIAuth {uriUserInfo = ""} = True
isUserInfoValid _                          = False

isHostValid :: URIAuth -> Bool
isHostValid URIAuth {uriRegName = ""} = False
isHostValid _                         = True

isPortValid :: URIAuth -> Bool
isPortValid URIAuth {uriPort = ""} = True
isPortValid URIAuth {uriPort = (':':p)} =
  case readMaybe p of
    Just i  -> i > (0 :: Integer) && i < 65536
    Nothing -> False
isPortValid _ = False
