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
      it "should set a single integer column" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| [
            { "op": "set", "path": "id", "value": "10" }
          ] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = []
          }

      it "should set a single integer column and return" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "set", "path": "id", "value": "10" }
          ] |]
          `shouldRespondWith`
          [json| [{"id":10,"name":"eddy"}] |]
          { matchStatus = 200
          , matchHeaders = ["Preference-Applied" <:> "return=representation"]
          }

      it "should set a single text column" $
        request methodPatch "/test_pgrst_patch?id=eq.2"
          [jsonPatchCT]
          [json| [
            { "op": "set", "path": "name", "value": "double d" }
          ] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = []
          }

      it "should set a single text column and return" $
        request methodPatch "/test_pgrst_patch?id=eq.2"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "set", "path": "name", "value": "double d" }
          ] |]
          `shouldRespondWith`
          [json| [{"id":2,"name":"double d"}] |]
          { matchStatus = 200
          , matchHeaders = ["Preference-Applied" <:> "return=representation"]
          }

      it "should set multiple columns and return" $
        request methodPatch "/test_pgrst_patch?id=eq.2"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "set", "path": "id", "value": "10" },
            { "op": "set", "path": "name", "value": "double d" }
          ] |]
          `shouldRespondWith`
          [json| [{"id":10,"name":"double d"}] |]
          { matchStatus = 200
          , matchHeaders = ["Preference-Applied" <:> "return=representation"]
          }
