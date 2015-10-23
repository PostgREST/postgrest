module Feature.AuthSpec where

-- {{{ Imports
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types

import SpecHelper
-- }}}

spec :: Spec
spec = beforeAll
  (clearTable "postgrest.auth") . afterAll_ (clearTable "postgrest.auth")
  $ around withApp
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

  it "hides tables from users with invalid JWT" $ do
    let auth = authHeaderJWT "ey9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 404

  it "recovers after 400 error with logged in user" $ do
    _ <- post "/authors_only" [json| { "owner": "jdoe", "secret": "test content" } |]
    let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9zdGdyZXN0X3Rlc3RfYXV0aG9yIiwiaWQiOiJqZG9lIn0.y4vZuu1dDdwAl0-S00MCRWRYMlJ5YAMSir6Es6WtWx0"
    _ <- request methodPost "/rpc/problem" [auth] ""
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200
