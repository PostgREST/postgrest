{-|
Module      : PostgREST.AuthSAML
Description : PostgREST SAML authentication functions.

This module provides functions to deal with the SAML authentication.

-}
{-# OPTIONS_GHC -Wwarn=deprecations #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module PostgREST.AuthSAML
  ( middleware
  ) where

import Crypto.PubKey.RSA.Types (PublicKey (..))

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BL
import           Data.ByteString.UTF8  as BSU
import qualified Data.Cache            as C
import           Data.IORef            (newIORef, atomicModifyIORef)
import qualified Data.Map              as Map
import qualified Data.Text             as T
import           Data.String           (String)
import           Network.Wai.SAML2
import qualified Crypto.Store.X509     as X509
import qualified Data.X509             as X509

import qualified Network.Wai           as Wai
import           Network.HTTP.Types    (status401)

import           PostgREST.AppState    (AppState (..), SAML2State (..))

import           Protolude

-- | Middleware for the SAML2 authentication.
-- TODO: Here we need to block access to the /rpc/login endpoint.
middleware :: AppState -> Wai.Middleware
middleware appState app =
  case stateSAML2 appState of
    Nothing -> app
    Just samlState ->
      saml2Callback samlConfig (handleSAML2Result samlState) app
      where
        samlConfig = saml2StateAppConfig samlState

-- | Discard the request and respond with an error message.
respondError :: BL.ByteString -> Wai.Response
respondError = Wai.responseLBS
    status401
    [("Content-Type", "text/plain")]

-- | For every SAML authentication error,
-- we want to log it and respond with a generic error message.
handleSamlError :: String -> IO Wai.Response
handleSamlError err = do
  putStrLn $ ("SAML2 Error: " :: String) ++ show err
  pure $ respondError "SAML authentication error. Check the server logs."

-- | Converts the original request to a request to the login endpoint.
-- NOTE: Is this a good idea?
redirectToLogin :: Wai.Request -> Text -> String -> IO Wai.Request
redirectToLogin req login_endpoint username = do

  newBody <- generateBody

  return req
    { Wai.requestHeaders = new_headers
    , Wai.requestBody = newBody
    , Wai.rawPathInfo = BSU.fromString $ T.unpack login_endpoint
    , Wai.pathInfo = filter (/= "") $ T.splitOn "/" login_endpoint
    , Wai.requestBodyLength = Wai.KnownLength
                            $ fromIntegral
                            $ S8.length rendered_form_data
    , Wai.requestMethod = "POST"
    }
  where
    new_headers = [("Content-Type", "application/x-www-form-urlencoded")]
    form_data = Map.fromList [ ("email", username)
                             , ("pass", "123")
                             ]
    rendered_form_data = renderFormData form_data
    generateBody = do
      ichunks <- newIORef [rendered_form_data]
      let rbody = atomicModifyIORef ichunks $ \case
                 [] -> ([], S8.empty)
                 x:y -> (y, x)
      return rbody

-- | Convert a map of form data to a bytestring that can be used as a request body.
renderFormData :: Map.Map String String -> ByteString
renderFormData d = S8.pack $ intercalate "&" $ map renderPair $ Map.toList d
  where
    renderPair (k, v) = k ++ "=" ++ v

-- | Modifies the request according to the results from the SAML2 validation.
handleSAML2Result :: SAML2State -> Either SAML2Error Result -> Wai.Middleware
handleSAML2Result samlState result' _app req respond =
  case result' of
    Right result -> do
      known_assertion <- tryRetrieveAssertion samlState (assertionId (assertion result))
      if known_assertion
      then respond =<< handleSamlError "Replay attack detected."
      else do
          -- NOTE: SAML Authentication success!
          let _ = readSignedCertificate ("" :: String)
          let user = fromMaybe "anon" $ extractUserName $ assertion result
          putStrLn $ "SAML login for username: " ++ user
          req' <- redirectToLogin req (saml2JwtEndpoint samlState) user
          storeAssertion samlState (assertionId (assertion result))
          _app req' respond
    Left err -> respond =<< handleSamlError (show err)

-- | Extracts the username from the assertion.
extractUserName :: Assertion -> Maybe String
extractUserName a =
  case find isWanted $ assertionAttributeStatement a of
    Just attr -> Just $ T.unpack $ attributeValue attr
    Nothing -> Nothing
  where
    isWanted attr = attributeName attr == "email"

-- | Checks if a given assertion ID is already known.
tryRetrieveAssertion :: SAML2State -> Text -> IO Bool
tryRetrieveAssertion samlState t = do
  isJust <$> C.lookup (saml2KnownIds samlState) t

-- | Store a known assertion ID in the cache.
storeAssertion :: SAML2State -> Text -> IO ()
storeAssertion samlState t = C.insert (saml2KnownIds samlState) t ()

-- | Read a signed certificate from a PEM string and extract its public key.
readSignedCertificate :: String -> Either String PublicKey
readSignedCertificate input =
  case (X509.readSignedObjectFromMemory $ BSU.fromString $ ensurePemFrame input) :: [X509.SignedCertificate] of
    [] -> Left "Invalid certificate."
    (x:_) -> case X509.signedObject $ X509.getSigned x of
      X509.Certificate _ _ _ _ _ _ (X509.PubKeyRSA pubKey) _ -> Right pubKey
      _ -> Left $ "Wrong type of certificate." ++ show x
  where
    ensurePemFrame x = case head x of
      Just '-' -> x
      _        -> "-----BEGIN CERTIFICATE-----\n" ++ x ++ "\n-----END CERTIFICATE-----"
