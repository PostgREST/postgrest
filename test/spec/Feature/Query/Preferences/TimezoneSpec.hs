module Feature.Query.Preferences.TimezoneSpec where

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWithConfig
spec withConfig = withConfig baseCfg $ do
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
    it "should throw error without handling" $
      request methodGet "/timestamps"
        [("Prefer", "timezone=Invalid/Timezone")]
        ""
        `shouldRespondWith`
        [json|{"code":"22023","details":null,"hint":null,"message":"invalid value for parameter \"TimeZone\": \"Invalid/Timezone\""}|]
        { matchStatus = 400 }

    it "should throw error with handling=strict" $
      request methodGet "/timestamps"
        [("Prefer", "handling=strict, timezone=Invalid/Timezone")]
        ""
        `shouldRespondWith`
        [json|{"code":"22023","details":null,"hint":null,"message":"invalid value for parameter \"TimeZone\": \"Invalid/Timezone\""}|]
        { matchStatus = 400 }

    it "should throw error with handling=lenient" $
      request methodGet "/timestamps"
        [("Prefer", "handling=lenient, timezone=Invalid/Timezone")]
        ""
        `shouldRespondWith`
        [json|{"code":"22023","details":null,"hint":null,"message":"invalid value for parameter \"TimeZone\": \"Invalid/Timezone\""}|]
        { matchStatus = 400 }
