module Feature.UpsertSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types

import SpecHelper
import Network.Wai (Application)

import Protolude hiding (get, put)
import Text.Heredoc

spec :: SpecWith Application
spec =
  describe "UPSERT" $ do
    context "with POST" $ do
      context "when Prefer: resolution=merge-duplicates is specified" $ do
        it "INSERTs and UPDATEs rows on pk conflict" $
          request methodPost "/tiobe_pls" [("Prefer", "return=representation"), ("Prefer", "resolution=merge-duplicates")]
            [json| [
              { "name": "Javascript", "rank": 6 },
              { "name": "Java", "rank": 2 },
              { "name": "C", "rank": 1 }
            ]|] `shouldRespondWith` [json| [
              { "name": "Javascript", "rank": 6 },
              { "name": "Java", "rank": 2 },
              { "name": "C", "rank": 1 }
            ]|]
            { matchStatus = 201
            , matchHeaders = ["Preference-Applied" <:> "resolution=merge-duplicates", matchContentTypeJson]
            }

        it "INSERTs and UPDATEs row on composite pk conflict" $
          request methodPost "/employees" [("Prefer", "return=representation"), ("Prefer", "resolution=merge-duplicates")]
            [json| [
              { "first_name": "Frances M.", "last_name": "Roe", "salary": "30000" },
              { "first_name": "Peter S.", "last_name": "Yang", "salary": 42000 }
            ]|] `shouldRespondWith` [json| [
              { "first_name": "Frances M.", "last_name": "Roe", "salary": "$30,000.00", "company": "One-Up Realty", "occupation": "Author" },
              { "first_name": "Peter S.", "last_name": "Yang", "salary": "$42,000.00", "company": null, "occupation": null }
            ]|]
            { matchStatus = 201
            , matchHeaders = ["Preference-Applied" <:> "resolution=merge-duplicates", matchContentTypeJson]
            }

      context "when Prefer: resolution=ignore-duplicates is specified" $ do
        it "INSERTs and ignores rows on pk conflict" $
          request methodPost "/tiobe_pls" [("Prefer", "return=representation"), ("Prefer", "resolution=ignore-duplicates")]
            [json|[
              { "name": "PHP", "rank": 9 },
              { "name": "Python", "rank": 10 }
            ]|] `shouldRespondWith` [json|[
              { "name": "PHP", "rank": 9 }
            ]|]
            { matchStatus = 201
            , matchHeaders = ["Preference-Applied" <:> "resolution=ignore-duplicates", matchContentTypeJson]
            }

        it "INSERTs and ignores rows on composite pk conflict" $
          request methodPost "/employees" [("Prefer", "return=representation"), ("Prefer", "resolution=ignore-duplicates")]
            [json|[
              { "first_name": "Daniel B.", "last_name": "Lyon", "salary": "72000", "company": null, "occupation": null },
              { "first_name": "Sara M.", "last_name": "Torpey", "salary": 60000, "company": "Burstein-Applebee", "occupation": "Soil scientist" }
            ]|] `shouldRespondWith` [json|[
              { "first_name": "Sara M.", "last_name": "Torpey", "salary": "$60,000.00", "company": "Burstein-Applebee", "occupation": "Soil scientist" }
            ]|]
            { matchStatus = 201
            , matchHeaders = ["Preference-Applied" <:> "resolution=ignore-duplicates", matchContentTypeJson]
            }

      it "succeeds if the table has only PK cols and no other cols" $ do
        request methodPost "/only_pk" [("Prefer", "return=representation"), ("Prefer", "resolution=ignore-duplicates")]
          [json|[ { "id": 1 }, { "id": 2 }, { "id": 3} ]|]
          `shouldRespondWith`
          [json|[ { "id": 3} ]|]
          { matchStatus = 201 ,
            matchHeaders = ["Preference-Applied" <:> "resolution=ignore-duplicates",
            matchContentTypeJson] }

        request methodPost "/only_pk" [("Prefer", "return=representation"), ("Prefer", "resolution=merge-duplicates")]
          [json|[ { "id": 1 }, { "id": 2 }, { "id": 4} ]|]
          `shouldRespondWith`
          [json|[ { "id": 1 }, { "id": 2 }, { "id": 4} ]|]
          { matchStatus = 201 ,
            matchHeaders = ["Preference-Applied" <:> "resolution=merge-duplicates",
            matchContentTypeJson] }

      it "succeeds and ignores the Prefer: resolution header(no Preference-Applied present) if the table has no PK" $
        request methodPost "/no_pk" [("Prefer", "return=representation"), ("Prefer", "resolution=merge-duplicates")]
          [json|[ { "a": "1", "b": "0" } ]|]
          `shouldRespondWith`
          [json|[ { "a": "1", "b": "0" } ]|] { matchStatus = 201 , matchHeaders = [matchContentTypeJson] }

      it "succeeds if not a single resource is created" $ do
        request methodPost "/tiobe_pls" [("Prefer", "return=representation"), ("Prefer", "resolution=ignore-duplicates")]
          [json|[ { "name": "Java", "rank": 1 } ]|] `shouldRespondWith`
          [json|[]|] { matchStatus = 201 , matchHeaders = [matchContentTypeJson] }
        request methodPost "/tiobe_pls" [("Prefer", "return=representation"), ("Prefer", "resolution=ignore-duplicates")]
          [json|[ { "name": "Java", "rank": 1 }, { "name": "C", "rank": 2 } ]|] `shouldRespondWith`
          [json|[]|] { matchStatus = 201 , matchHeaders = [matchContentTypeJson] }

    context "with PUT" $ do
      context "Restrictions" $ do
        it "fails if Range is specified" $
          request methodPut "/tiobe_pls?name=eq.Javascript" [("Range", "0-5")]
            [str| [ { "name": "Javascript", "rank": 1 } ]|] `shouldRespondWith` 400

        it "fails if limit is specified" $
          put "/tiobe_pls?name=eq.Javascript&limit=1"
            [str| [ { "name": "Javascript", "rank": 1 } ]|] `shouldRespondWith` 400

        it "fails if offset is specified" $
          put "/tiobe_pls?name=eq.Javascript&offset=1"
            [str| [ { "name": "Javascript", "rank": 1 } ]|] `shouldRespondWith` 400

        it "fails if the payload has more than one row" $
          put "/tiobe_pls?name=eq.Go"
            [str| [ { "name": "Go", "rank": 19 }, { "name": "Swift", "rank": 12 } ]|] `shouldRespondWith` 400

        it "fails if not all columns are specified" $ do
          put "/tiobe_pls?name=eq.Go"
            [str| [ { "name": "Go" } ]|] `shouldRespondWith` 400
          put "/employees?first_name=eq.Susan&last_name=eq.Heidt"
            [str| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "48000" } ]|] `shouldRespondWith` 400

        it "rejects every other filter than pk cols eq's" $ do
          put "/tiobe_pls?rank=eq.19" [str| [ { "name": "Go", "rank": 19 } ]|] `shouldRespondWith` 405
          put "/tiobe_pls?id=not.eq.Java" [str| [ { "name": "Go", "rank": 19 } ]|] `shouldRespondWith` 405
          put "/tiobe_pls?id=in.(Go)" [str| [ { "name": "Go", "rank": 19 } ]|] `shouldRespondWith` 405
          put "/tiobe_pls?and=(id.eq.Go)" [str| [ { "name": "Go", "rank": 19 } ]|] `shouldRespondWith` 405

        it "fails if not all composite key cols are specified as eq filters" $ do
          put "/employees?first_name=eq.Susan"
            [str| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "48000", "company": "GEX", "occupation": "Railroad engineer" } ]|]
            `shouldRespondWith` 405
          put "/employees?last_name=eq.Heidt"
            [str| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "48000", "company": "GEX", "occupation": "Railroad engineer" } ]|]
            `shouldRespondWith` 405

      it "fails if the uri primary key doesn't match the payload primary key" $ do
        put "/tiobe_pls?name=eq.MATLAB"
          [str| [ { "name": "Perl", "rank": 17 } ]|] `shouldRespondWith` 400
        put "/employees?first_name=eq.Wendy&last_name=eq.Anderson"
          [str| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "48000", "company": "GEX", "occupation": "Railroad engineer" } ]|] `shouldRespondWith` 400

      it "fails if the table has no PK" $
        put "/no_pk?a=eq.one&b=eq.two" [str| [ { "a": "one", "b": "two" } ]|] `shouldRespondWith` 405

      context "Inserting row" $ do
        it "succeeds on table with single pk col" $ do
          get "/tiobe_pls?name=eq.Go" `shouldRespondWith` "[]"
          put "/tiobe_pls?name=eq.Go" [str| [ { "name": "Go", "rank": 19 } ]|] `shouldRespondWith` 204
          get "/tiobe_pls?name=eq.Go" `shouldRespondWith` [json| [ { "name": "Go", "rank": 19 } ]|] { matchHeaders = [matchContentTypeJson] }

        it "succeeds on table with composite pk" $ do
          get "/employees?first_name=eq.Susan&last_name=eq.Heidt"
            `shouldRespondWith` "[]"
          put "/employees?first_name=eq.Susan&last_name=eq.Heidt"
            [str| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "48000", "company": "GEX", "occupation": "Railroad engineer" } ]|]
            `shouldRespondWith` 204
          get "/employees?first_name=eq.Susan&last_name=eq.Heidt"
            `shouldRespondWith`
            [json| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "$48,000.00", "company": "GEX", "occupation": "Railroad engineer" } ]|]
            { matchHeaders = [matchContentTypeJson] }

        it "succeeds if the table has only PK cols and no other cols" $ do
          get "/only_pk?id=eq.10" `shouldRespondWith` "[]"
          put "/only_pk?id=eq.10" [str|[ { "id": 10 } ]|] `shouldRespondWith` 204
          get "/only_pk?id=eq.10" `shouldRespondWith` [json|[ { "id": 10 } ]|] { matchHeaders = [matchContentTypeJson] }

      context "Updating row" $ do
        it "succeeds on table with single pk col" $ do
          get "/tiobe_pls?name=eq.Go" `shouldRespondWith` [json|[ { "name": "Go", "rank": 19 } ]|] { matchHeaders = [matchContentTypeJson] }
          put "/tiobe_pls?name=eq.Go" [str| [ { "name": "Go", "rank": 13 } ]|] `shouldRespondWith` 204
          get "/tiobe_pls?name=eq.Go" `shouldRespondWith` [json| [ { "name": "Go", "rank": 13 } ]|] { matchHeaders = [matchContentTypeJson] }

        it "succeeds on table with composite pk" $ do
          get "/employees?first_name=eq.Susan&last_name=eq.Heidt"
            `shouldRespondWith`
            [json| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "$48,000.00", "company": "GEX", "occupation": "Railroad engineer" } ]|]
            { matchHeaders = [matchContentTypeJson] }
          put "/employees?first_name=eq.Susan&last_name=eq.Heidt"
            [str| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "60000", "company": "Gamma Gas", "occupation": "Railroad engineer" } ]|]
            `shouldRespondWith` 204
          get "/employees?first_name=eq.Susan&last_name=eq.Heidt"
            `shouldRespondWith`
            [json| [ { "first_name": "Susan", "last_name": "Heidt", "salary": "$60,000.00", "company": "Gamma Gas", "occupation": "Railroad engineer" } ]|]
            { matchHeaders = [matchContentTypeJson] }

        it "succeeds if the table has only PK cols and no other cols" $ do
          get "/only_pk?id=eq.10" `shouldRespondWith` [json|[ { "id": 10 } ]|] { matchHeaders = [matchContentTypeJson] }
          put "/only_pk?id=eq.10" [str|[ { "id": 10 } ]|] `shouldRespondWith` 204
          get "/only_pk?id=eq.10" `shouldRespondWith` [json|[ { "id": 10 } ]|] { matchHeaders = [matchContentTypeJson] }

      it "works with return=representation and vnd.pgrst.object+json" $
        request methodPut "/tiobe_pls?name=eq.Ruby"
          [("Prefer", "return=representation"), ("Accept", "application/vnd.pgrst.object+json")]
          [str| [ { "name": "Ruby", "rank": 11 } ]|]
          `shouldRespondWith` [json|{ "name": "Ruby", "rank": 11 }|] { matchHeaders = [matchContentTypeSingular] }
