module Feature.Auth.AuthSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "authorization" $ do
  let single = ("Accept","application/vnd.pgrst.object+json")

  it "denies access to tables that anonymous does not own" $
    get "/authors_only" `shouldRespondWith`
      [json| {
        "hint":null,
        "details":null,
        "code":"42501",
        "message":"permission denied for table authors_only"} |]
      { matchStatus = 401
      , matchHeaders = ["WWW-Authenticate" <:> "Bearer"]
      }

  it "denies access to tables that postgrest_test_author does not own" $
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIn0.Xod-F15qsGL0WhdOCr2j3DdKuTw9QJERVgoFD3vGaWA" in
    request methodGet "/private_table" [auth] ""
      `shouldRespondWith`
      [json| {
        "hint":null,
        "details":null,
        "code":"42501",
        "message":"permission denied for table private_table"} |]
      { matchStatus = 403
      , matchHeaders = []
      }

  it "denies execution on functions that anonymous does not own" $
    post "/rpc/privileged_hello" [json|{"name": "anonymous"}|] `shouldRespondWith` 401

  it "allows execution on a function that postgrest_test_author owns" $
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIn0.Xod-F15qsGL0WhdOCr2j3DdKuTw9QJERVgoFD3vGaWA" in
    request methodPost "/rpc/privileged_hello" [auth] [json|{"name": "jdoe"}|]
      `shouldRespondWith` [json|"Privileged hello to jdoe"|]
      { matchStatus = 200
      , matchHeaders = [matchContentTypeJson]
      }

  it "returns jwt functions as jwt tokens" $
    request methodPost "/rpc/login" [single]
      [json| { "id": "jdoe", "pass": "1234" } |]
      `shouldRespondWith` [json| {"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xuYW1lIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.KO-0PGp_rU-utcDBP6qwdd-Th2Fk-ICVt01I7QtTDWs"} |]
        { matchStatus = 200
        , matchHeaders = [matchContentTypeSingular]
        }

  it "sql functions can encode custom and standard claims" $
    request methodPost  "/rpc/jwt_test" [single] "{}"
      `shouldRespondWith` [json| {"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJqb2UiLCJzdWIiOiJmdW4iLCJhdWQiOiJldmVyeW9uZSIsImV4cCI6MTMwMDgxOTM4MCwibmJmIjoxMzAwODE5MzgwLCJpYXQiOjEzMDA4MTkzODAsImp0aSI6ImZvbyIsInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdCIsImh0dHA6Ly9wb3N0Z3Jlc3QuY29tL2ZvbyI6dHJ1ZX0.G2REtPnOQMUrVRDA9OnkPJTd8R0tf4wdYOlauh1E2Ek"} |]
        { matchStatus = 200
        , matchHeaders = [matchContentTypeSingular]
        }

  it "sql functions can read custom and standard claims variables" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJmdW4iLCJqdGkiOiJmb28iLCJuYmYiOjEzMDA4MTkzODAsImV4cCI6OTk5OTk5OTk5OSwiaHR0cDovL3Bvc3RncmVzdC5jb20vZm9vIjp0cnVlLCJpc3MiOiJqb2UiLCJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWF0IjoxMzAwODE5MzgwfQ.V5fEpXfpb7feqwVqlcDleFdKu86bdwU2cBRT4fcMhXg"
    request methodPost "/rpc/reveal_big_jwt" [auth] "{}"
      `shouldRespondWith` [json|[{"iss":"joe","sub":"fun","exp":9999999999,"nbf":1300819380,"iat":1300819380,"jti":"foo","http://postgrest.com/foo":true}]|]

  it "allows users with permissions to see their tables" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.B-lReuGNDwAlU1GOC476MlO0vAt9JNoHIlxg2vwMaO0"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  it "works with tokens which have extra fields" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIiwia2V5MSI6InZhbHVlMSIsImtleTIiOiJ2YWx1ZTIiLCJrZXkzIjoidmFsdWUzIiwiYSI6MSwiYiI6MiwiYyI6M30.b0eglDKYEmGi-hCvD-ddSqFl7vnDO5qkUaviaHXm3es"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  -- this test will stop working 9999999999s after the UNIX EPOCH
  it "succeeds with an unexpired token" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjk5OTk5OTk5OTksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UifQ.Dpss-QoLYjec5OTsOaAc3FNVsSjA89wACoV-0ra3ClA"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  it "fails with an expired token" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE0NDY2NzgxNDksInJvbGUiOiJwb3N0Z3Jlc3RfdGVzdF9hdXRob3IiLCJpZCI6Impkb2UifQ.f8__E6VQwYcDqwHmr9PG03uaZn8Zh1b0vbJ9DYS0AdM"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` [json| {"message":"JWT expired","code":"PGRST301","hint":null,"details":null} |]
        { matchStatus = 401
        , matchHeaders = [
            "WWW-Authenticate" <:>
            "Bearer error=\"invalid_token\", error_description=\"JWT expired\""
          ]
        }

  it "hides tables from users with invalid JWT" $ do
    let auth = authHeaderJWT "ey9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` [json| {"message":"JWSError (CompactDecodeError Invalid number of parts: Expected 3 parts; got 2)","code":"PGRST301","hint":null,"details":null} |]
        { matchStatus = 401
        , matchHeaders = [
            "WWW-Authenticate" <:>
            "Bearer error=\"invalid_token\", error_description=\"JWSError (CompactDecodeError Invalid number of parts: Expected 3 parts; got 2)\""
          ]
        }

  it "should fail when jwt contains no claims" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.e30.CUIP5V9thWsGGFsFyGijSZf1fJMfarLHI9CEJL-TGNk"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 401

  it "hides tables from users with JWT that contain no claims about role" $ do
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6Impkb2UifQ.RVlZDaSyKbFPvxUf3V_NQXybfRB4dlBIkAUQXVXLUAI"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 401

  it "recovers after 401 error with logged in user" $ do
    _ <- post "/authors_only" [json| { "owner": "jdoe", "secret": "test content" } |]
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.B-lReuGNDwAlU1GOC476MlO0vAt9JNoHIlxg2vwMaO0"
    _ <- request methodPost "/rpc/problem" [auth] ""
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  describe "custom pre-request proc acting on id claim" $ do

    it "able to switch to postgrest_test_author role (id=1)" $
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MX0.gKw7qI50i9hMrSJW8BlTpdMEVmMXJYxlAqueGqpa_mE" in
      request methodPost "/rpc/get_current_user" [auth]
        [json| {} |]
         `shouldRespondWith` [json|"postgrest_test_author"|]
          { matchStatus = 200
          , matchHeaders = []
          }

    it "able to switch to postgrest_test_default_role (id=2)" $
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6Mn0.nwzjMI0YLvVGJQTeoCPEBsK983b__gxdpLXisBNaO2A" in
      request methodPost "/rpc/get_current_user" [auth]
        [json| {} |]
         `shouldRespondWith` [json|"postgrest_test_default_role"|]
          { matchStatus = 200
          , matchHeaders = []
          }

    it "raises error (id=3)" $
      let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6M30.OGxEJAf60NKZiTn-tIb2jy4rqKs_ZruLGWZ40TjrJsM" in
      request methodPost "/rpc/get_current_user" [auth]
        [json| {} |]
         `shouldRespondWith` [json|{"hint":"Please contact administrator","details":null,"code":"P0001","message":"Disabled ID --> 3"}|]
          { matchStatus = 400
          , matchHeaders = []
          }

  it "allows 'Bearer' and 'bearer' as authentication schemes" $ do
    let token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.B-lReuGNDwAlU1GOC476MlO0vAt9JNoHIlxg2vwMaO0"
    request methodGet "/authors_only" [authHeader "Bearer" token] ""
      `shouldRespondWith` 200
    request methodGet "/authors_only" [authHeader "bearer" token] ""
      `shouldRespondWith` 200
