module Feature.AuthSpec where

-- {{{ Imports
import Text.Heredoc
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types

import SpecHelper
import Network.Wai (Application)

import Protolude hiding (get)
-- }}}

spec :: SpecWith Application
spec = describe "authorization" $ do
  let single = ("Accept","application/vnd.pgrst.object+json")

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
    request methodPost "/rpc/login" [single]
      [json| { "id": "jdoe", "pass": "1234" } |]
      `shouldRespondWith` ResponseMatcher {
          matchBody = Just [json| {"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xuYW1lIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.P2G9EVSVI22MWxXWFuhEYd9BZerLS1WDlqzdqplM15s"} |]
        , matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
        }

  it "sql functions can encode custom and standard claims" $
    request methodPost  "/rpc/jwt_test" [single] "{}"
      `shouldRespondWith` ResponseMatcher {
          matchBody = Just [json| {"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJqb2UiLCJzdWIiOiJmdW4iLCJhdWQiOiJldmVyeW9uZSIsImV4cCI6MTMwMDgxOTM4MCwibmJmIjoxMzAwODE5MzgwLCJpYXQiOjEzMDA4MTkzODAsImp0aSI6ImZvbyIsInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdCIsImh0dHA6Ly9wb3N0Z3Jlc3QuY29tL2ZvbyI6dHJ1ZX0.IHF16ZSU6XTbOnUWO8CCpUn2fJwt8P00rlYVyXQjpWc"} |]
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
      `shouldRespondWith` ResponseMatcher {
          matchBody = Nothing
        , matchStatus = 401
        , matchHeaders = [
            "WWW-Authenticate" <:>
            "Bearer error=\"invalid_token\", error_description=\"JWT expired\""
          ]
        }

  it "hides tables from users with invalid JWT" $ do
    let auth = authHeaderJWT "ey9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` ResponseMatcher {
          matchBody = Nothing
        , matchStatus = 401
        , matchHeaders = [
            "WWW-Authenticate" <:>
            "Bearer error=\"invalid_token\", error_description=\"JWT invalid\""
          ]
        }

  it "should fail when jwt contains no claims" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.e30.lu-rG8aSCiw-aOlN0IxpRGz5r7Jwq7K9r3tuMPUpytI"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 401

  it "hides tables from users with JWT that contain no claims about role" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6Impkb2UifQ.Jneso9X519Vh0z7i9PbXIu7W1HEoq9RRw9BBbyQKFCQ"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 401

  it "recovers after 401 error with logged in user" $ do
    _ <- post "/authors_only" [json| { "owner": "jdoe", "secret": "test content" } |]
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
    _ <- request methodPost "/rpc/problem" [auth] ""
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  describe "custom pre-request proc acting on id claim" $ do

    it "able to switch to postgrest_test_author role (id=1)" $
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MX0.mI2HNoOum6xM3sc4oHLxU4yLv-_WV5W1kqBfY_wEvLw" in
      request methodPost "/rpc/get_current_user" [auth]
        [json| {} |]
         `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|"postgrest_test_author"|]
          , matchStatus = 200
          , matchHeaders = []
          }

    it "able to switch to postgrest_test_default_role (id=2)" $
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6Mn0.W7jLsG-zswM91AJkCvZeIMHrnz7_6ceY2jnscVl3Yhk" in
      request methodPost "/rpc/get_current_user" [auth]
        [json| {} |]
         `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|"postgrest_test_default_role"|]
          , matchStatus = 200
          , matchHeaders = []
          }

    it "raises error (id=3)" $
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6M30.15Gy8PezQhJIaHYDJVLa-Gmz9T3sJnW66EKAYIsXc7c" in
      request methodPost "/rpc/get_current_user" [auth]
        [json| {} |]
         `shouldRespondWith` ResponseMatcher {
            matchBody    = Just [str|{"hint":"Please contact administrator","details":null,"code":"P0001","message":"Disabled ID --> 3"}|]
          , matchStatus = 400
          , matchHeaders = []
          }
