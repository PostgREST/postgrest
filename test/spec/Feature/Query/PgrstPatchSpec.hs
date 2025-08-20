module Feature.Query.PgrstPatchSpec where

import Network.Wai (Application)
import Test.Hspec  hiding (pendingWith)

import Network.HTTP.Types
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

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

      it "should err on invalid operator" $
        request methodPatch "/test_pgrst_patch?id=eq.2"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "invalid_op", "path": "id", "value": "10" }
          ] |]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"All objects should contain 3 key-vals: 'op','path' and 'value', where op and path must be a string"} |]
          { matchStatus = 400
          , matchHeaders = [matchContentTypeJson]
          }

      it "should err when op is other type instead of string" $
        request methodPatch "/test_pgrst_patch?id=eq.2"
          [jsonPatchCT]
          [json| [
            { "op": 403, "path": "id", "value": "20" }
          ] |]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"All objects should contain 3 key-vals: 'op','path' and 'value', where op and path must be a string"} |]
          { matchStatus = 400
          , matchHeaders = [matchContentTypeJson]
          }

      it "should err when path is other type instead of string" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT]
          [json| [
            { "op": "incr", "path": 403, "value": "20" }
          ] |]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"All objects should contain 3 key-vals: 'op','path' and 'value', where op and path must be a string"} |]
          { matchStatus = 400
          , matchHeaders = [matchContentTypeJson]
          }

      it "should err when value other type instead of number" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "set", "path": "id", "value": "string" }
          ] |]
          `shouldRespondWith`
          [json| {"code":"22P02","details":null,"hint":null,"message":"invalid input syntax for type integer: \"string\""} |]
          { matchStatus = 400
          , matchHeaders = [matchContentTypeJson]
          }

      it "should err on wrong json patch format" $
        request methodPatch "/test_pgrst_patch?id=eq.2"
          [jsonPatchCT]
          [json| { "non-json-patch": "wrong-format" }|]
          `shouldRespondWith`
          [json| {"code":"PGRST102","details":null,"hint":null,"message":"All objects should contain 3 key-vals: 'op','path' and 'value', where op and path must be a string"} |]
          { matchStatus = 400
          , matchHeaders = [matchContentTypeJson]
          }

      it "should err when column not found" $
        request methodPatch "/test_pgrst_patch?id=eq.2"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "set", "path": "non-existent-column", "value": "double d" }
          ] |]
          `shouldRespondWith`
          [json| {"code":"PGRST204","details":null,"hint":null,"message":"Could not find the 'non-existent-column' column of 'test_pgrst_patch' in the schema cache"} |]
          { matchStatus = 400
          , matchHeaders = [matchContentTypeJson]
          }

      it "should return content-length on success" $
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "set", "path": "id", "value": "10" }
          ] |]
          `shouldRespondWith`
          [json| [{"id":10,"name":"eddy"}] |]
          { matchStatus = 200
          , matchHeaders = ["Content-Length" <:> "25",
                            "Preference-Applied" <:> "return=representation"]
          }

      it "should work with count=exact" $ -- added for coverage, not much useful to have?
        request methodPatch "/test_pgrst_patch?id=eq.3"
          [jsonPatchCT, ("Prefer","return=representation, count=exact")]
          [json| [
            { "op": "set", "path": "id", "value": "10" }
          ] |]
          `shouldRespondWith`
          [json| [{"id":10,"name":"eddy"}] |]
          { matchStatus = 200
          , matchHeaders = ["Content-Length" <:> "25",
                            "Content-Range" <:> "0-0/1",
                            "Preference-Applied" <:> "return=representation, count=exact"]
          }

      it "should set on multiple conditions in where logic" $
        request methodPatch "/test_pgrst_patch?id=eq.3&name=eq.eddy"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "set", "path": "id", "value": "10" }
          ] |]
          `shouldRespondWith`
          [json| [{"id":10,"name":"eddy"}] |]
          { matchStatus = 200
          , matchHeaders = ["Preference-Applied" <:> "return=representation"]
          }

      it "should return empty on empty body" $
        request methodPatch "/test_pgrst_patch"
          [jsonPatchCT]
          [json| [] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = []
          }

      it "should only update the columns in the &columns query param" $
        request methodPatch "/test_pgrst_patch?id=eq.2&columns=name"
          [jsonPatchCT, ("Prefer","return=representation")]
          [json| [
            { "op": "set", "path": "id", "value": "10" },
            { "op": "set", "path": "name", "value": "double d" }
          ] |]
          `shouldRespondWith`
          [json| [{"id":2,"name":"double d"}] |]
          { matchStatus = 200
          , matchHeaders = ["Preference-Applied" <:> "return=representation"]
          }
