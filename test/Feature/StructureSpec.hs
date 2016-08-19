module Feature.StructureSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Network.HTTP.Types

import SpecHelper

import Network.Wai (Application)
import Network.Wai.Test (SResponse(simpleHeaders))

spec :: SpecWith Application
spec = do

  describe "OpenAPI" $ do
    it "root path returns a valid openapi spec" $
      validateOpenApiResponse [("Accept", "application/openapi+json")]

    it "should respond to openapi request on none root path with 415" $
      request methodGet "/items"
              (acceptHdrs "application/openapi+json") ""
        `shouldRespondWith` 415

  describe "Allow header" $ do

    it "includes read/write verbs for writeable table" $ do
      r <- request methodOptions "/items" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "GET,POST,PATCH,DELETE"

    it "includes read verbs for read-only table" $ do
      r <- request methodOptions "/has_count_column" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "GET"
