module Feature.MultipleSchemaSpec where

import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Protolude       hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "multiple schemas in single instance" $
    it "read table in default schema" $ do
      get "/table" `shouldRespondWith`
        [json|[
          {"id":1,"value":"value1"},
          {"id":2,"value":"value2"}
        ]|]
        { matchStatus = 200, matchHeaders = [matchContentTypeJson] }
