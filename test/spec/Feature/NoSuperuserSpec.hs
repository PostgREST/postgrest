module Feature.NoSuperuserSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude

spec :: SpecWith ((), Application)
spec =
  describe "No Superuser" $ do
    it "proves that the authenticator role is not a superuser" $ do
      request methodGet "/rpc/is_superuser"
          mempty
          ""
        `shouldRespondWith`
          "false"
          { matchStatus = 200 }
