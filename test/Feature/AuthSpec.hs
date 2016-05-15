module Feature.AuthSpec where

-- {{{ Imports
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types

import SpecHelper
import Network.Wai (Application)
-- }}}

spec :: SpecWith Application
spec = describe "authorization" $ do

  it "denies access to tables that anonymous does not own" $
    get "/authors_only" `shouldRespondWith` ResponseMatcher {
        matchBody = Just [json| {
          "hint":null,
          "details":null,
          "code":"42501",
          "message":"permission denied for relation authors_only"} |]
      , matchStatus = 401
      , matchHeaders = ["WWW-Authenticate" <:> "Bearer"]
      }

  it "denies access to tables that postgrest_test_author does not own" $
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0" in
    request methodGet "/private_table" [auth] ""
      `shouldRespondWith` ResponseMatcher {
        matchBody = Just [json| {
          "hint":null,
          "details":null,
          "code":"42501",
          "message":"permission denied for relation private_table"} |]
      , matchStatus = 403
      , matchHeaders = []
      }

  it "returns jwt functions as jwt tokens" $
    post "/rpc/login" [json| { "id": "jdoe", "pass": "1234" } |]
      `shouldRespondWith` ResponseMatcher {
          matchBody = Just [json| {"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"} |]
        , matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
        }

  it "sql functions can encode custom and standard claims" $
    post "/rpc/jwt_test" "{}"
      `shouldRespondWith` ResponseMatcher {
          matchBody = Just [json| {"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJmdW4iLCJqdGkiOiJmb28iLCJuYmYiOjEzMDA4MTkzODAsImV4cCI6MTMwMDgxOTM4MCwiaHR0cDovL3Bvc3RncmVzdC5jb20vZm9vIjp0cnVlLCJpc3MiOiJqb2UiLCJyb2xlIjoicG9zdGdyZXN0X3Rlc3QiLCJpYXQiOjEzMDA4MTkzODAsImF1ZCI6ImV2ZXJ5b25lIn0._tQCF79-ZZGMlLktd3csM_bVaiMg7A8YvIb6K2hcu5w"} |]
        , matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
        }

  it "sql functions can read custom and standard claims variables" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJmdW4iLCJqdGkiOiJmb28iLCJuYmYiOjEzMDA4MTkzODAsImV4cCI6OTk5OTk5OTk5OSwiaHR0cDovL3Bvc3RncmVzdC5jb20vZm9vIjp0cnVlLCJpc3MiOiJqb2UiLCJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWF0IjoxMzAwODE5MzgwLCJhdWQiOiJldmVyeW9uZSJ9.AQmCA7CMScvfaDRMqRPeUY6eNf--69gpW-kxaWfq9X0"
    request methodPost "/rpc/reveal_big_jwt" [auth] "{}"
      `shouldRespondWith` [json| [
          {"sub":"fun", "jti":"foo", "nbf":1300819380, "exp":9999999999,
          "http://postgrest.com/foo":true, "iss":"joe", "iat":1300819380,
          "aud":"everyone"}] |]

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
