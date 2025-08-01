module Feature.Query.PgrstPatchSpec where

import Network.Wai (Application)
import Test.Hspec  hiding (pendingWith)

import Network.HTTP.Types
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude hiding (get)

spec :: SpecWith ((), Application)
spec = do
  let jsonPatchCT = ("Content-Type","application/vnd.pgrst.patch+json")
  describe "Partial Document Update using JSON Patch" $ do
    context "Update Single Row" $ do
      it "should incr a single column" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| [
            { "op": "incr", "path": "id", "value": 10 }
          ] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = []
          }

      it "should incr a single column and return" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "incr", "path": "id", "value": 10 }
          ] |]
          `shouldRespondWith`
          [json| [{"id":13,"emp_no":30,"card_no":300}] |]
          { matchStatus = 200
          , matchHeaders = ["Preference-Applied" <:> "return=representation"]
          }

      it "should patch multiple columns in one go" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| [
            { "op": "incr", "path": "id",     "value": 10 },
            { "op": "incr", "path": "emp_no", "value": 20 },
            { "op": "incr", "path": "card_no","value": 30 }
          ] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = []
          }

      it "should patch multiple columns in one go and return" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "incr", "path": "id",     "value": 10 },
            { "op": "incr", "path": "emp_no", "value": 20 },
            { "op": "incr", "path": "card_no","value": 30 }
          ] |]
          `shouldRespondWith`
          [json| [{"id":13,"emp_no":50,"card_no":330}] |]
          { matchStatus = 200
          , matchHeaders = ["Preference-Applied" <:> "return=representation"]
          }

      it "should patch multiple columns with negative values" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "incr", "path": "id",     "value": -10 },
            { "op": "incr", "path": "emp_no", "value": -20 },
            { "op": "incr", "path": "card_no","value": -30 }
          ] |]
          `shouldRespondWith`
          [json| [{"id":-7,"emp_no":10,"card_no":270}] |]
          { matchStatus = 200
          , matchHeaders = ["Preference-Applied" <:> "return=representation"]
          }

      it "should err on unknown operator" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| [
            { "op": "unknown_op", "path": "id", "value": 10 }
          ] |]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"Error in $[0]: Unknown Pgrst Patch operation \"unknown_op\""} |]
          { matchStatus = 400
          , matchHeaders = []
          }

      it "should err when op is other type instead of string" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| [
            { "op": 9999, "path": "id", "value": "20" }
          ] |]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"Error in $[0]: Expected JSON string for \"op\""} |]
          { matchStatus = 400
          , matchHeaders = []
          }

      it "should err when path is other type instead of string" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| [
            { "op": "incr", "path": 9999, "value": "20" }
          ] |]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"Error in $[0]: Expected JSON string for \"path\""} |]
          { matchStatus = 400
          , matchHeaders = []
          }

      it "should err when value other type instead of number" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| [
            { "op": "incr", "path": "id", "value": "20" }
          ] |]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"Error in $[0]: Expected JSON number for \"value\""} |]
          { matchStatus = 400
          , matchHeaders = []
          }

      it "should err on wrong json patch format" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| { "non-json-patch": "wrong-format" }|]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"Error in $: parsing [] failed, expected Array, but encountered Object"} |]
          { matchStatus = 400
          , matchHeaders = []
          }
