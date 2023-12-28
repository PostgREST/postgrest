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
-- TODO: Here we could block access to the /rpc/login endpoint,
-- but function privileges can protect the function from being called directly.
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

-- | Converts the original request into a request to the login endpoint.
-- NOTE: Is this a good idea?
redirectToLoginEndpoint :: Wai.Request -> Text -> Text -> IO Wai.Request
redirectToLoginEndpoint req login_endpoint role = do

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
    form_data = Map.fromList [ ("target_role", T.unpack role)
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
    Left err -> respond =<< handleSamlError (show err)
    Right result -> do
      known_assertion <- tryRetrieveAssertionID samlState (assertionId (assertion result))
      if known_assertion
      then respond =<< handleSamlError "Replay attack detected."
      else do
          -- NOTE: SAML Authentication success!
          let
            attributes = extractAttributes $ assertion result
            user = fromMaybe "tester" $ Map.lookup "role" attributes

          putStrLn ("SAML parameters: " ++ show (Map.toList attributes) :: String)
          putStrLn $ "SAML login for username: " ++ T.unpack user

          req' <- redirectToLoginEndpoint req (saml2JwtEndpoint samlState) user
          storeAssertionID samlState (assertionId (assertion result))
          _app req' respond
  where
    _readCertificateFromSignature :: Result -> Maybe Text
    _readCertificateFromSignature result = do
      keyInfo <- signatureKeyInfo $ responseSignature $ response result
      return $ encodeCertificate $ keyInfoCertificate keyInfo

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

-- | Extracts all relevant atrtibutes from the assertion.
-- This includes all assertion attribute statements along with the name id.
extractAttributes :: Assertion -> Map Text Text
extractAttributes assertion' = Map.insert "name_id" name_id attributes
  where
    simplifyAttribute :: AssertionAttribute -> (Text, Text)
    simplifyAttribute attr = (attributeName attr, attributeValue attr)

    attributes = Map.fromList
               $ map simplifyAttribute
               $ assertionAttributeStatement assertion'

    name_id = nameIDValue $ subjectNameID $ assertionSubject assertion'


-- | Checks if a given assertion ID is already known.
tryRetrieveAssertionID :: SAML2State -> Text -> IO Bool
tryRetrieveAssertionID samlState t =
  isJust <$> C.lookup (saml2KnownIds samlState) t

-- | Store a known assertion ID in the cache.
storeAssertionID :: SAML2State -> Text -> IO ()
storeAssertionID samlState t = C.insert (saml2KnownIds samlState) t ()
