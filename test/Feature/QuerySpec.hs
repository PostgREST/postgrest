module Feature.QuerySpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Hasql as H
import Hasql.Postgres as H
import Control.Monad (void)
import Data.Text(Text)

import SpecHelper
import Data.Monoid

testSet :: IO ()
testSet = do
  clearTable "items" >> clearTable "no_pk"
  createItems 15
  pool <- H.acquirePool pgSettings testPoolOpts
  void . liftIO $ H.session pool $ H.tx Nothing $ do
    H.unitEx $ insertNoPk "xyyx" "u"
    H.unitEx $ insertNoPk "xYYx" "v"

  where
    insertNoPk :: Text -> Text -> H.Stmt H.Postgres
    insertNoPk = [H.stmt|insert into "1".no_pk (a, b) values (?,?)|]

spec :: Spec
spec = do
  beforeAll (clearTable "items" >> createItems 15)
   . beforeAll (clearTable "no_pk" >> createNulls 2)
   . afterAll_ (clearTable "items" >> clearTable "no_pk")
   . around withApp $ do
  describe "Querying a nonexistent table" $
    it "causes a 404" $
      get "/faketable" `shouldRespondWith` 404

  describe "Filtering response" $ do
    it "matches with equality" $
      get "/items?id=eq.5"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":5}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-0/1"]
        }

    it "matches items IN" $
      get "/items?id=in.1,3,5"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":3},{"id":5}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "matches with like" $ do
      get "/no_pk?a=like.*yx" `shouldRespondWith` [json|
        [{"a":"xyyx","b":"u"}]|]
      get "/no_pk?a=like.xy*" `shouldRespondWith` [json|
        [{"a":"xyyx","b":"u"}]|]
      get "/no_pk?a=like.*YY*" `shouldRespondWith` [json|
        [{"a":"xYYx","b":"v"}]|]

    it "matches with ilike" $ do
      get "/no_pk?a=ilike.xy*&order=b.asc" `shouldRespondWith` [json|
        [{"a":"xyyx","b":"u"},{"a":"xYYx","b":"v"}]|]
      get "/no_pk?a=ilike.*YY*&order=b.asc" `shouldRespondWith` [json|
        [{"a":"xyyx","b":"u"},{"a":"xYYx","b":"v"}]|]

  describe "ordering response" $ do
    it "by a column asc" $
      get "/items?id=lte.2&order=id.asc"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":1},{"id":2}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }
    it "by a column desc" $
      get "/items?id=lte.2&order=id.desc"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just [json| [{"id":2},{"id":1}] |]
        , matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }

    it "by a column asc with nulls last" $
      get "/no_pk?order=a.asc.nullslast"
        `shouldRespondWith` ResponseMatcher {
          matchBody = Just $ "[{\"a\":\"1\",\"b\":\"0\"}"
                          <> ",{\"a\":\"2\",\"b\":\"0\"}"
                          <> ",{\"a\":null,\"b\":null}]"

        , matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "by a column desc with nulls first" $
      get "/no_pk?order=a.desc.nullsfirst"
        `shouldRespondWith` ResponseMatcher {
          matchBody = Just $ "[{\"a\":null,\"b\":null}"
                          <> ",{\"a\":\"2\",\"b\":\"0\"}"
                          <> ",{\"a\":\"1\",\"b\":\"0\"}]"
        , matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "by a column desc with nulls last" $
      get "/no_pk?order=a.desc.nullslast"
        `shouldRespondWith` ResponseMatcher {
          matchBody = Just $ "[{\"a\":\"2\",\"b\":\"0\"}"
                          <> ",{\"a\":\"1\",\"b\":\"0\"}"
                          <> ",{\"a\":null,\"b\":null}]"

        , matchStatus = 200
        , matchHeaders = ["Content-Range" <:> "0-2/3"]
        }

    it "without other constraints" $
      get "/items?order=asc.id" `shouldRespondWith` 200

  describe "Canonical location" $ do
    it "Sets Content-Location with alphabetized params" $
      get "/no_pk?b=eq.1&a=eq.1"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just "[]"
        , matchStatus  = 200
        , matchHeaders = ["Content-Location" <:> "/no_pk?a=eq.1&b=eq.1"]
        }

    it "Omits question mark when there are no params" $
      get "/simple_pk"
        `shouldRespondWith` ResponseMatcher {
          matchBody    = Just "[]"
        , matchStatus  = 200
        , matchHeaders = ["Content-Location" <:> "/simple_pk"]
        }
