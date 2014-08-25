
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Feature.InsertSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test (SResponse(simpleBody))

import SpecHelper

import qualified Data.Aeson as JSON
import Data.Aeson ((.:))
import Data.Maybe (fromJust)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

data IncPK = IncPK {
  incId :: Int
, incNullableStr :: Maybe String
, incStr :: String
, incInsert :: String
} deriving (Show)

instance JSON.FromJSON IncPK where
  parseJSON (JSON.Object r) = IncPK <$>
    r .: "id" <*>
    r .: "nullable_string" <*>
    r .: "non_nullable_string" <*>
    r .: "inserted_at"
  parseJSON _ = mzero

spec :: Spec
spec = around appWithFixture $
  describe "Posting new record" $
    context "with no pk supplied" $
      context "into a table with auto-incrementing pk" $
        it "succeeds with 201 and link" $ do
          post "/auto_incrementing_pk" [json| { "non_nullable_string":"not null"} |]
            `shouldRespondWith` ResponseMatcher {
              matchBody    = Nothing,
              matchStatus  = 201,
              matchHeaders = [("Location", "/auto_incrementing_pk?id=eq.1")]
            }
          r <- get "/auto_incrementing_pk?id=eq.1"
          let [record] = fromJust (JSON.decode $ simpleBody r :: Maybe [IncPK])
          liftIO $ do
            incStr record `shouldBe` "not null"
            incNullableStr record `shouldBe` Nothing
