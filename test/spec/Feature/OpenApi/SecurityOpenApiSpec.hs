module Feature.OpenApi.SecurityOpenApiSpec where

import Control.Lens ((^?))

import Data.Aeson.Lens
import Data.Aeson.QQ

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Test.Hspec     hiding (pendingWith)
import Test.Hspec.Wai

import Protolude hiding (get)

spec :: SpecWith ((), Application)
spec =
  describe "Security active" $
    it "includes security and security definitions" $ do
      r <- simpleBody <$> get "/"

      let sec = r ^? key "security"
          secDef = r ^? key "securityDefinitions"

      liftIO $ do

        sec `shouldBe` Just
          [aesonQQ|
            [
              { "JWT": [] }
            ]
          |]

        secDef `shouldBe` Just
          [aesonQQ|
            {
              "JWT": {
                "description": "Add the token prepending \"Bearer \" (without quotes) to it",
                "in": "header",
                "name": "Authorization",
                "type": "apiKey"
              }
            }
          |]
