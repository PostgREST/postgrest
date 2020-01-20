module Feature.MultipleSchemaSpec where

import Network.Wai (Application)
import Network.HTTP.Types

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "multiple schemas in single instance" $ do
    it "read table in default schema(v1) if no schema is selected via header" $
      request methodGet "/table" [] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value1"},
          {"id":2,"value":"value2"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "read table from default schema(v1) after explicitly passing it in the header" $
      request methodGet "/table" [("Accept-Version", "v1")] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value1"},
          {"id":2,"value":"value2"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "read table in other schema(v2)" $
      request methodGet "/table" [("Accept-Version", "v2")] "" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value3"},
          {"id":2,"value":"value4"}
        ]|]
        {
          matchStatus = 200
        , matchHeaders = [matchContentTypeJson]
        }

    it "read table in unkown schema" $
      request methodGet "/table" [("Accept-Version", "unkown")] "" `shouldRespondWith`
        ""
        {
          matchStatus = 404
        , matchHeaders = []
        }
