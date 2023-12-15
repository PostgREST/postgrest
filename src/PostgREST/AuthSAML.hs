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

import qualified Crypto.Hash           as H
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy  as BL
import           Data.ByteString.UTF8  as BSU
import qualified Data.Cache            as C
import           Data.IORef            (newIORef, atomicModifyIORef)
import qualified Data.Map              as Map
import qualified Data.Text             as T
import           Data.String           (String)

import           Network.Wai.SAML2
import           Network.Wai.SAML2.Response

import qualified Network.Wai           as Wai
import           Network.HTTP.Types    (status401)

import           PostgREST.AppState    (AppState (..), SAML2State (..))

import           Protolude
import           Network.Wai.SAML2.KeyInfo (KeyInfo(keyInfoCertificate))

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
redirectToLogin :: Wai.Request -> Text -> Text -> IO Wai.Request
redirectToLogin req login_endpoint issuer = do

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
    form_data = Map.fromList [ ("issuer", T.unpack issuer)
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
      known_assertion <- tryRetrieveAssertionID samlState (assertionId (assertion result))
      if known_assertion
      then respond =<< handleSamlError "Replay attack detected."
      else do
          -- NOTE: SAML Authentication success!
          let user = fromMaybe "anon" $ Map.lookup "email" $ extractAttributes $ assertion result

          putStrLn $ "SAML login for username: " ++ T.unpack user

          case signatureKeyInfo $ responseSignature $ response result of
            Nothing -> respond =<< handleSamlError "No certificate found in SAML Response, aborting..."
            Just keyInfo -> do
              let certificate = encodeCertificate $ keyInfoCertificate keyInfo

              putStrLn $ "SAML login with certificate: " ++ T.unpack certificate

              req' <- redirectToLogin req (saml2JwtEndpoint samlState) certificate
              storeAssertionID samlState (assertionId (assertion result))
              _app req' respond
    Left err -> respond =<< handleSamlError (show err)

-- | Encode a certificate to be used as a JWT parameter.
-- Here we encode it as SHA256 because passing the raw certificate
-- over requests will hit encoding issues.
encodeCertificate :: BSU.ByteString -> Text
encodeCertificate = T.pack
                  . show
                  . H.hashWith H.SHA256
                  . BSU.fromString
                  . T.unpack
                  . T.replace "\n" ""
                  . T.pack
                  . BSU.toString

-- | Extracts the username from the assertion.
extractAttributes :: Assertion -> Map Text Text
extractAttributes = Map.fromList
                  . map simplifyAttribute
                  . assertionAttributeStatement
  where
    simplifyAttribute :: AssertionAttribute -> (Text, Text)
    simplifyAttribute attr = (attributeName attr, attributeValue attr)

-- | Checks if a given assertion ID is already known.
tryRetrieveAssertionID :: SAML2State -> Text -> IO Bool
tryRetrieveAssertionID samlState t = do
  isJust <$> C.lookup (saml2KnownIds samlState) t

-- | Store a known assertion ID in the cache.
storeAssertionID :: SAML2State -> Text -> IO ()
storeAssertionID samlState t = C.insert (saml2KnownIds samlState) t ()
