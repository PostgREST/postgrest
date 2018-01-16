module Feature.UpsertSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types

import SpecHelper
import Network.Wai (Application)

import Protolude hiding (get)

spec :: SpecWith Application
spec =
  describe "UPSERT" $
    context "POST with Prefer headers" $ do
      context "when Prefer: resolution=merge-duplicates is specified" $ do
        it "does upsert on pk conflict" $
          request methodPost "/tiobe_pls" [("Prefer", "return=representation"), ("Prefer", "resolution=merge-duplicates")]
            [json| [
              { "name": "Javascript", "rank": 6 },
              { "name": "Java", "rank": 5 }
            ]|] `shouldRespondWith` [json| [
              { "name": "Javascript", "rank": 6 },
              { "name": "Java", "rank": 5 }
            ]|]
            { matchStatus = 201
            , matchHeaders = [matchContentTypeJson]
            }

        it "does upsert on composite pk conflict" $
          request methodPost "/employees" [("Prefer", "return=representation"), ("Prefer", "resolution=merge-duplicates")]
            [json| [
              { "first_name": "Frances M.", "last_name": "Roe", "salary": "30000" },
              { "first_name": "Peter S.", "last_name": "Yang", "salary": 42000 }
            ]|] `shouldRespondWith` [json| [
              { "first_name": "Frances M.", "last_name": "Roe", "salary": "$30,000.00", "company": "One-Up Realty", "occupation": "Author" },
              { "first_name": "Peter S.", "last_name": "Yang", "salary": "$42,000.00", "company": null, "occupation": null }
            ]|]
            { matchStatus = 201
            , matchHeaders = [matchContentTypeJson]
            }

      context "when Prefer: resolution=ignore-duplicates is specified" $ do
        it "ignores records on pk conflict" $ do
          request methodPost "/tiobe_pls" [("Prefer", "return=representation"), ("Prefer", "resolution=ignore-duplicates")]
            [json|[
              { "name": "PHP", "rank": 9 },
              { "name": "Python", "rank": 10 }
            ]|] `shouldRespondWith` [json|[
              { "name": "PHP", "rank": 9 }
            ]|]
            { matchStatus = 201
            , matchHeaders = [matchContentTypeJson]
            }
          get "/tiobe_pls?rank=gte.9" `shouldRespondWith`
            [json| [{ "name": "PHP", "rank": 9 }] |]
            { matchHeaders = [matchContentTypeJson] }

        it "ignores records on composite pk conflict" $ do
          request methodPost "/employees" [("Prefer", "return=representation"), ("Prefer", "resolution=ignore-duplicates")]
            [json|[
              { "first_name": "Daniel B.", "last_name": "Lyon", "salary": "72000", "company": null, "occupation": null },
              { "first_name": "Sara M.", "last_name": "Torpey", "salary": 60000, "company": "Burstein-Applebee", "occupation": "Soil scientist" }
            ]|] `shouldRespondWith` [json|[
              { "first_name": "Sara M.", "last_name": "Torpey", "salary": "$60,000.00", "company": "Burstein-Applebee", "occupation": "Soil scientist" }
            ]|]
            { matchStatus = 201
            , matchHeaders = [matchContentTypeJson]
            }
          get "/employees?first_name=eq.Daniel B.&last_name=eq.Lyon" `shouldRespondWith`
            [json| [{ "first_name": "Daniel B.", "last_name": "Lyon", "salary": "$36,000.00", "company": "Dubrow's Cafeteria", "occupation": "Packer" }] |]
            { matchHeaders = [matchContentTypeJson] }
