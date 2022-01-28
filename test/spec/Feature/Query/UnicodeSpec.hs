module Feature.Query.UnicodeSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Reading and writing to unicode schema and table names" $
    it "Can read and write values" $ do
      get "/%D9%85%D9%88%D8%A7%D8%B1%D8%AF"
        `shouldRespondWith` "[]"

      request methodPost "/%D9%85%D9%88%D8%A7%D8%B1%D8%AF"
          [("Prefer", "tx=commit"), ("Prefer", "return=representation")]
          [json| { "هویت": 1 } |]
        `shouldRespondWith`
          [json| [{ "هویت": 1 }] |]
          { matchStatus = 201 }

      get "/%D9%85%D9%88%D8%A7%D8%B1%D8%AF"
        `shouldRespondWith`
          [json| [{ "هویت": 1 }] |]

      request methodDelete "/%D9%85%D9%88%D8%A7%D8%B1%D8%AF"
          [("Prefer", "tx=commit")]
          ""
        `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = [matchHeaderAbsent hContentType]
          }
