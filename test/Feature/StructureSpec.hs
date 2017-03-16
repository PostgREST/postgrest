module Feature.StructureSpec where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Network.HTTP.Types

import Control.Lens ((^?))
import Data.Aeson.Lens
import Data.Aeson.QQ

import SpecHelper

import Network.Wai (Application)
import Network.Wai.Test (SResponse(..))

import Protolude hiding (get)

spec :: SpecWith Application
spec = do

  describe "OpenAPI" $ do
    it "root path returns a valid openapi spec" $
      validateOpenApiResponse [("Accept", "application/openapi+json")]

    it "should respond to openapi request on none root path with 415" $
      request methodGet "/items"
              (acceptHdrs "application/openapi+json") ""
        `shouldRespondWith` 415

    describe "RPC" $

      it "includes a representative function with parameters" $ do
        r <- simpleBody <$> get "/"
        let ref = r ^? key "paths" . key "/rpc/varied_arguments"
                     . key "post"  . key "parameters"
                     . nth 1       . key "schema"
                     . key "$ref"  . _String
            args = r ^? key "definitions" . key "(rpc) varied_arguments"

        liftIO $ do
          ref `shouldBe` Just "#/definitions/(rpc) varied_arguments"
          args `shouldBe` Just
            [aesonQQ|
              {
                "required": [
                  "double",
                  "varchar",
                  "boolean",
                  "date",
                  "money",
                  "enum"
                ],
                "properties": {
                  "double": {
                    "format": "double precision",
                    "type": "string"
                  },
                  "varchar": {
                    "format": "character varying",
                    "type": "string"
                  },
                  "boolean": {
                    "format": "boolean",
                    "type": "boolean"
                  },
                  "date": {
                    "format": "date",
                    "type": "string"
                  },
                  "money": {
                    "format": "money",
                    "type": "string"
                  },
                  "enum": {
                    "format": "test.enum_menagerie_type",
                    "type": "string"
                  },
                  "integer": {
                    "format": "integer",
                    "type": "integer"
                  }
                },
                "type": "object"
              }
            |]

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
