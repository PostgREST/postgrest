module Feature.Query.Preferences.TimezoneSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "test Prefer: timezone" $ do
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
