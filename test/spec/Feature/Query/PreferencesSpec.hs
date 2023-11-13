module Feature.Query.PreferencesSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "test prefer headers and preference-applied headers" $ do

    context "check behaviour of Prefer: handling=strict" $ do
      it "throws error when handling=strict and invalid prefs are given" $
        request methodGet  "/items" [("Prefer", "handling=strict, anything")] ""
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throw error when handling=strict and invalid prefs are given with multiples in separate prefers" $
        request methodGet  "/items" [("Prefer", "handling=strict"),("Prefer","something, else")] ""
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: something, else","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throws error with post request" $
        request methodPost  "/organizations?select=*"
          [("Prefer","return=representation, handling=strict, anything")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throws error with rpc" $
        request methodPost "/rpc/overloaded_unnamed_param"
          [("Content-Type", "application/json"), ("Prefer", "handling=strict, anything")]
          [json|{}|]
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

    context "check behaviour of Prefer: handling=lenient" $ do
      it "does not throw error when handling=lenient and invalid prefs" $
        request methodGet  "/items" [("Prefer", "handling=lenient, anything")] ""
          `shouldRespondWith` 200

      it "does not throw error when handling=lenient and invalid prefs in multiples prefers" $
        request methodGet  "/items" [("Prefer", "handling=lenient"), ("Prefer", "anything")] ""
          `shouldRespondWith` 200

      it "does not throw error with post request" $
        request methodPost  "/organizations?select=*"
          [("Prefer","return=representation, handling=lenient, anything")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith`
          [json|[{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}]|]
          { matchStatus  = 201
          , matchHeaders = [ matchContentTypeJson ]
          }

      it "does not throw error with rpc" $
        request methodPost "/rpc/overloaded_unnamed_param"
          [("Content-Type", "application/json"), ("Prefer", "handling=lenient, anything")]
          [json|{}|]
          `shouldRespondWith`
          [json| 1 |]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
           }

    context "test Prefer: timezone=America/Los_Angeles" $ do
      it "should change timezone with handling=strict" $
        request methodGet "/timestamps"
          [("Prefer", "handling=strict, timezone=America/Los_Angeles")]
          ""
          `shouldRespondWith`
          [json|[{"t":"2023-10-18T05:37:59.611-07:00"}, {"t":"2023-10-18T07:37:59.611-07:00"}, {"t":"2023-10-18T09:37:59.611-07:00"}]|]
          { matchStatus = 200
          , matchHeaders = [matchContentTypeJson
                           , "Preference-Applied" <:> "handling=strict, timezone=America/Los_Angeles"]}

      it "should change timezone without handling=strict" $
        request methodGet "/timestamps"
          [("Prefer", "timezone=America/Los_Angeles")]
          ""
          `shouldRespondWith`
          [json|[{"t":"2023-10-18T05:37:59.611-07:00"}, {"t":"2023-10-18T07:37:59.611-07:00"}, {"t":"2023-10-18T09:37:59.611-07:00"}]|]
          { matchStatus = 200
          , matchHeaders = [matchContentTypeJson
                           , "Preference-Applied" <:> "timezone=America/Los_Angeles"] }

    context "test Prefer: timezone=Invalid/Timezone" $ do
      it "should throw error with handling=strict" $
        request methodGet "/timestamps"
          [("Prefer", "handling=strict, timezone=Invalid/Timezone")]
          ""
          `shouldRespondWith`
          [json|{"code":"PGRST122","details":"Invalid preferences: timezone=Invalid/Timezone","hint":null,"message":"Invalid preferences given with handling=strict"}|]
          { matchStatus = 400 }

      it "should return with default timezone without handling or with handling=lenient" $ do
        request methodGet "/timestamps"
          [("Prefer", "timezone=Invalid/Timezone")]
          ""
          `shouldRespondWith`
          [json|[{"t":"2023-10-18T12:37:59.611+00:00"}, {"t":"2023-10-18T14:37:59.611+00:00"}, {"t":"2023-10-18T16:37:59.611+00:00"}]|]
          { matchStatus = 200
          , matchHeaders = [matchContentTypeJson]}

        request methodGet "/timestamps"
          [("Prefer", "handling=lenient, timezone=Invalid/Timezone")]
          ""
          `shouldRespondWith`
          [json|[{"t":"2023-10-18T12:37:59.611+00:00"}, {"t":"2023-10-18T14:37:59.611+00:00"}, {"t":"2023-10-18T16:37:59.611+00:00"}]|]
          { matchStatus = 200
          , matchHeaders = [matchContentTypeJson
                           , "Preference-Applied" <:> "handling=lenient"]}
