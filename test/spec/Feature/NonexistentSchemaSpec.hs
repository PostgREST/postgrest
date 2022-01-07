module Feature.NonexistentSchemaSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai

import Protolude hiding (get)

spec :: SpecWith ((), Application)
spec =
  describe "Non existent api schema" $ do
    it "succeeds when requesting root path" $
      get "/" `shouldRespondWith` 200

    it "gives 404 when requesting a nonexistent table in this nonexistent schema" $
      get "/nonexistent_table" `shouldRespondWith` 404
