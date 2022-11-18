module Feature.Query.ExtraOperatorsSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "db-extra-operators" $ do
    it "can use the custom fuzzy operator" $ do
      get "/users?name=fuzzy.Angel" `shouldRespondWith`
        [json| [{"id":1,"name":"Angela Martin"}] |]
        { matchHeaders = [matchContentTypeJson] }
      get "/users?or=(name.fuzzy.Michel,name.fuzzy.Angel)" `shouldRespondWith`
        [json| [{"id":1,"name":"Angela Martin"}, {"id":2,"name":"Michael Scott"}] |]
        { matchHeaders = [matchContentTypeJson] }

    it "can use the custom ltreefts operator" $
      -- doing path @ 'Astro*% & !pictures@' <- urlencoded
      -- taken from the examples in https://www.postgresql.org/docs/current/ltree.html
      get "/ltree_sample?path=ltreefts.Astro*%25%20%26%20!pictures@" `shouldRespondWith`
        [json|[
          {"path":"Top.Science.Astronomy"},
          {"path":"Top.Science.Astronomy.Astrophysics"},
          {"path":"Top.Science.Astronomy.Cosmology"},
          {"path":"Top.Hobbies.Amateurs_Astronomy"}] |]
        { matchHeaders = [matchContentTypeJson] }
