module Feature.Query.PgSafeUpdateSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get, put)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Enabling pg-safeupdate" $ do
    context "Full table update" $
      it "does not update and throws no error if no condition is present" $
        request methodPatch "/safe_update"
            [("Prefer", "count=exact")]
            [json| {"name": "New name"} |]
          `shouldRespondWith`
            ""
            { matchStatus  = 404
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/0" ]
            }

    context "Full table delete" $ do
      it "does not delete and throws error if no condition is present" $
        request methodDelete "/safe_delete" [] mempty
          `shouldRespondWith`
            [json|{
              "code": "21000",
              "details": null,
              "hint": null,
              "message": "DELETE requires a WHERE clause"
            }|]
            { matchStatus  = 400 }

      it "allows full table delete if a filter is present" $
        request methodDelete "/safe_delete?id=gt.0"
            [("Prefer", "count=exact")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/3" ]
            }

disabledSpec :: SpecWith ((), Application)
disabledSpec =
  describe "Disabling pg-safeupdate" $ do
    context "Full table update" $
      it "does not update and does not throw error if no condition is present" $
        request methodPatch "/unsafe_update"
            [("Prefer", "count=exact")]
            [json| {"name": "New name"} |]
          `shouldRespondWith`
            ""
            { matchStatus  = 404
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/0" ]
            }

    context "Full table delete" $
      it "deletes and does not throw error if no condition is present" $ do
        request methodDelete "/unsafe_delete"
            [("Prefer", "count=exact")]
            mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/3" ]
            }
