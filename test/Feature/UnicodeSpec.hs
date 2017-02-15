module Feature.UnicodeSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai (Application)
import Control.Monad (void)

import SpecHelper

import Protolude hiding (get)

spec :: SpecWith Application
spec =
  describe "Reading and writing to unicode schema and table names" $
    it "Can read and write values" $ do
      get "/%D9%85%D9%88%D8%A7%D8%B1%D8%AF"
        `shouldRespondWith` "[]"

      void $ post "/%D9%85%D9%88%D8%A7%D8%B1%D8%AF"
        [json| { "هویت": 1 } |]

      get "/%D9%85%D9%88%D8%A7%D8%B1%D8%AF"
        `shouldRespondWith` [json| [{ "هویت": 1 }] |]
        { matchHeaders = [matchContentTypeJson] }
