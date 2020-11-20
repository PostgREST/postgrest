module Feature.OptionsSpec where

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "Allow header" $ do
  it "includes read/write verbs for writeable table" $ do
    r <- request methodOptions "/items" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET,POST,PATCH,DELETE"

  it "includes read verbs for read-only table" $ do
    r <- request methodOptions "/has_count_column" [] ""
    liftIO $
      simpleHeaders r `shouldSatisfy`
        matchHeader "Allow" "GET"

