module Feature.Query.PipelineModeSpec where

import Network.HTTP.Types
import Protolude hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config (AppConfig (..))
import SpecHelper

spec :: SpecWithConfig
spec withConfig = withConfig baseCfg{configDbPipelineMode = True} $
  describe "db-pipeline-mode = true" $ do
    it "handles a simple GET" $
      get "/items?id=eq.5"
        `shouldRespondWith` [json| [{"id":5}] |]
          { matchHeaders = ["Content-Range" <:> "0-0/*"] }

    it "handles a POST (rolled back by test config)" $
      request
        methodPost
        "/menagerie"
        [("Prefer", "return=representation")]
        [json| [{
          "integer": 42, "double": 3.14, "varchar": "pipeline"
        , "boolean": true, "date": "1900-01-01", "money": "$1.00"
        , "enum": "foo"
        }] |]
        `shouldRespondWith` 201

    it "propagates a query error under pipeline mode" $
      get "/faketable"
        `shouldRespondWith` 404

    it "invokes an RPC" $
      get "/rpc/is_superuser"
        `shouldRespondWith` "false"
          { matchStatus = 200 }
