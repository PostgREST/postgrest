module Feature.DeleteSpec where

import Data.Pool
import Test.Hspec
import Test.Hspec.Wai
import Text.Heredoc

import SpecHelper
import PostgREST.Types (DbStructure(..))
import qualified Hasql.Connection  as H

import Network.HTTP.Types

spec :: DbStructure -> Pool H.Connection -> Spec
spec struct pool = beforeAll resetDb
  . around (withApp cfgDefault struct pool) $
  describe "Deleting" $ do
    context "existing record" $ do
      it "succeeds with 204 and deletion count" $
        request methodDelete "/items?id=eq.1" [] ""
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Nothing
          , matchStatus  = 204
          , matchHeaders = ["Content-Range" <:> "*/1"]
          }

      it "actually clears items ouf the db" $ do
        _ <- request methodDelete "/items?id=lt.15" [] ""
        get "/items"
          `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|[{"id":15}]|]
          , matchStatus  = 200
          , matchHeaders = ["Content-Range" <:> "0-0/1"]
          }

    context "known route, unknown record" $
      it "fails with 404" $
        request methodDelete "/items?id=eq.101" [] "" `shouldRespondWith` 404

    context "totally unknown route" $
      it "fails with 404" $
        request methodDelete "/foozle?id=eq.101" [] "" `shouldRespondWith` 404
