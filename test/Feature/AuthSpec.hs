module Feature.AuthSpec where

-- {{{ Imports
import Data.Pool
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import qualified Hasql.Connection  as H

import SpecHelper
import PostgREST.Types (DbStructure(..))
-- }}}

spec :: DbStructure -> Pool H.Connection -> Spec
spec struct pool = around (withApp cfgDefault struct pool)
  $ describe "authorization" $ do

  it "hides tables that anonymous does not own" $
    get "/authors_only" `shouldRespondWith` 404

  it "returns jwt functions as jwt tokens" $
    post "/rpc/login" [json| { "id": "jdoe", "pass": "1234" } |]
      `shouldRespondWith` ResponseMatcher {
          matchBody = Just [json| {"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"} |]
        , matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "application/json"]
        }

  it "allows users with permissions to see their tables" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  it "works with tokens which have extra fields" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIiwia2V5MSI6InZhbHVlMSIsImtleTIiOiJ2YWx1ZTIiLCJrZXkzIjoidmFsdWUzIiwiYSI6MSwiYiI6MiwiYyI6M30.GfydCh-F4wnM379xs0n1zUgalwJIsb6YoBapCo8HlFk"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  -- this test will stop working 9999999999s after the UNIX EPOCH
  it "succeeds with an unexpired token" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjk5OTk5OTk5OTksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UifQ.QaPPLWTuyydMu_q7H4noMT7Lk6P4muet1OpJXF6ofhc"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  it "fails with an expired token" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE0NDY2NzgxNDksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UifQ.enk_qZ_u6gZsXY4R8bREKB_HNExRpM0lIWSLktk9JJQ"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 400

  it "hides tables from users with invalid JWT" $ do
    let auth = authHeaderJWT "ey9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 400

  it "should fail when jwt contains no claims" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.e30.MKYc_lOECtB0LJOiykilAdlHodB-I0_id2qHKq35dmc"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 400

  it "hides tables from users with JWT that contain no claims about role" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6Impkb2UifQ.zyohGMnrDy4_8eJTl6I2AUXO3MeCCiwR24aGWRkTE9o"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 400

  it "recovers after 400 error with logged in user" $ do
    _ <- post "/authors_only" [json| { "owner": "jdoe", "secret": "test content" } |]
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
    _ <- request methodPost "/rpc/problem" [auth] ""
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200
