module Feature.CorsSpec where

-- {{{ Imports
import Data.Pool
import Test.Hspec
import Test.Hspec.Wai
import Network.Wai.Test (SResponse(simpleHeaders, simpleBody))
import qualified Data.ByteString.Lazy as BL
import qualified Hasql.Connection  as H

import SpecHelper
import PostgREST.Types (DbStructure(..))

import Network.HTTP.Types
-- }}}

spec :: DbStructure -> Pool H.Connection -> Spec
spec struct pool = around (withApp cfgDefault struct pool) $ describe "CORS" $ do
    let preflightHeaders = [
          ("Accept", "*/*"),
          ("Origin", "http://example.com"),
          ("Access-Control-Request-Method", "POST"),
          ("Access-Control-Request-Headers", "Foo,Bar") ]
    let normalCors = [
          ("Host", "localhost:3000"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:32.0) Gecko/20100101 Firefox/32.0"),
          ("Origin", "http://localhost:8000"),
          ("Accept", "text/csv, */*; q=0.01"),
          ("Accept-Language", "en-US,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", "http://localhost:8000/"),
          ("Connection", "keep-alive") ]

    describe "preflight request" $ do
      it "replies naively and permissively to preflight request" $ do
        r <- request methodOptions "/items" preflightHeaders ""
        liftIO $ do
          let respHeaders = simpleHeaders r
          respHeaders `shouldSatisfy` matchHeader
            "Access-Control-Allow-Origin"
            "http://example.com"
          respHeaders `shouldSatisfy` matchHeader
            "Access-Control-Allow-Credentials"
            "true"
          respHeaders `shouldSatisfy` matchHeader
            "Access-Control-Allow-Methods"
            "GET, POST, PATCH, DELETE, OPTIONS, HEAD"
          respHeaders `shouldSatisfy` matchHeader
            "Access-Control-Allow-Headers"
            "Authentication, Foo, Bar, Accept, Accept-Language, Content-Language"
          respHeaders `shouldSatisfy` matchHeader
            "Access-Control-Max-Age"
            "86400"

      it "suppresses body in response" $ do
        r <- request methodOptions "/" preflightHeaders ""
        liftIO $ simpleBody r `shouldBe` ""

    describe "regular request" $
      it "exposes necesssary response headers" $ do
        r <- request methodGet "/items" [("Origin", "http://example.com")] ""
        liftIO $ simpleHeaders r `shouldSatisfy` matchHeader
          "Access-Control-Expose-Headers"
          "Content-Encoding, Content-Location, Content-Range, Content-Type, \
            \Date, Location, Server, Transfer-Encoding, Range-Unit"

    describe "postflight request" $
      it "allows INFO body through even with CORS request headers present" $ do
        r <- request methodOptions "/items" normalCors ""
        liftIO $ do
          simpleHeaders r `shouldSatisfy` matchHeader
            "Access-Control-Allow-Origin" "\\*"
          simpleBody r `shouldSatisfy` not . BL.null
