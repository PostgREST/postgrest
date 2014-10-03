{-# LANGUAGE OverloadedStrings #-}

module Feature.CorsSpec where

-- {{{ Imports
import Test.Hspec
import Test.Hspec.Wai
import Network.Wai.Test (SResponse(simpleHeaders))

import SpecHelper

import Network.HTTP.Types
-- }}}

spec :: Spec
spec = around appWithFixture $
  describe "CORS" $
    it "replies naively and permissively to preflight request" $ do
      r <- request methodOptions "/"
        [
          ("Accept", "*/*")
        , ("Origin", "http://example.com")
        , ("Access-Control-Request-Method", "POST")
        , ("Access-Control-Request-Headers", "Foo,Bar")
        ] ""
      liftIO $ do
        let respHeaders = simpleHeaders r
        respHeaders `shouldSatisfy` matchHeader
          "Access-Control-Allow-Origin"
          "http://example.com"
        respHeaders `shouldSatisfy` matchHeader
          "Access-Control-Allow-Credentials"
          "true"
        respHeaders `shouldSatisfy` matchHeader
          "Access-Control-Allow-Methods"
          "GET, POST, PUT, PATCH, DELETE, OPTIONS, HEAD"
        respHeaders `shouldSatisfy` matchHeader
          "Access-Control-Allow-Headers"
          "Authentication, Foo, Bar, Accept, Accept-Language, Content-Language"
        respHeaders `shouldSatisfy` matchHeader
          "Access-Control-Max-Age"
          "86400"
