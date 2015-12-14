module Feature.VersionSpec where

-- {{{ Imports
import Test.Hspec
import Test.Hspec.Wai

import Hasql as H
import Hasql.Postgres as P

import SpecHelper
import PostgREST.Types (DbStructure(..))
-- }}}

spec :: DbStructure -> H.Pool P.Postgres -> Spec
spec struct pool = around (withApp cfgTestV2Schema struct pool)
  $ describe "versioning" $ do

  it "hides tables not in v1 or v2 schemas" $
    get "/private_schema_view" `shouldRespondWith` 404

  it "can see a new view in v2" $
    get "/new_v2" `shouldRespondWith` 200

  it "can continue to see a route in v1" $
    get "/items" `shouldRespondWith` 200
