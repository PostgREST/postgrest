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
    context "Full table update" $ do
      it "does not work when no limit query or filter is given" $
        request methodPatch "/safe_update"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            [json| {"name": "New name"} |]
          `shouldRespondWith`
            ""
            { matchStatus  = 404
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/0" ]
            }

    context "full table delete" $ do
      it "throws error if no filter is present" $
        request methodDelete "/safe_delete"
            [("Prefer", "tx=commit"), ("Prefer", "count=exact")]
            mempty
          `shouldRespondWith`
            [json|{
              "code": "21000",
              "details": null,
              "hint": null,
              "message": "DELETE requires a WHERE clause"
            }|]
            { matchStatus  = 400 }

      it "allows full table delete if a filter is present" $
        request methodDelete "/safe_delete?id=gt.0" [("Prefer", "count=exact")] mempty
          `shouldRespondWith`
            ""
            { matchStatus  = 204
            , matchHeaders = [ matchHeaderAbsent hContentType
                             , "Content-Range" <:> "*/3" ]
            }
