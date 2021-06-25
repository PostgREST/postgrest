module Feature.IgnoreAclOpenApiSpec where

import Control.Lens ((^?))

import Data.Aeson.Lens
import Data.Aeson.QQ

import Network.HTTP.Types
import Network.Wai        (Application)
import Network.Wai.Test   (SResponse (..))

import Test.Hspec     hiding (pendingWith)
import Test.Hspec.Wai

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "OpenAPI Ignore ACL" $ do
  it "root path returns a valid openapi spec" $ do
    validateOpenApiResponse [("Accept", "application/openapi+json")]
    request methodHead "/" (acceptHdrs "application/openapi+json") ""
      `shouldRespondWith` "" { matchStatus  = 200 }

  describe "table" $ do

    it "includes privileged table even if user does not have permission" $ do
      r <- simpleBody <$> get "/"
      let tableTag = r ^? key "paths" . key "/authors_only"
                     . key "post" . key "tags"
                     . nth 0

      liftIO $ tableTag `shouldBe` Just [aesonQQ|"authors_only"|]

  describe "RPC" $ do

    it "includes privileged function even if user does not have permission" $ do
      r <- simpleBody <$> get "/"
      let funcTag = r ^? key "paths" . key "/rpc/privileged_hello"
                    . key "post" . key "tags"
                    . nth 0

      liftIO $ funcTag `shouldBe` Just [aesonQQ|"(rpc) privileged_hello"|]
