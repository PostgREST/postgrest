module Feature.SwitchingFnSpec where


import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Text.Heredoc
import SpecHelper
import Network.Wai (Application)

spec :: SpecWith Application
spec =
  describe "switching function works on id=1" $ do
      it "switches to postgrest_test_author on id=1" $
        let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6MX0.mI2HNoOum6xM3sc4oHLxU4yLv-_WV5W1kqBfY_wEvLw" in
        request methodPost "/rpc/get_current_user" [auth]
          [json| {} |]
           `shouldRespondWith` ResponseMatcher {
              matchBody    = Just [str|"postgrest_test_author"|]
            , matchStatus = 200
            , matchHeaders = []
            }

      it "switches to postgrest_test_default_role on id=2" $
        let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6Mn0.W7jLsG-zswM91AJkCvZeIMHrnz7_6ceY2jnscVl3Yhk" in
        request methodPost "/rpc/get_current_user" [auth]
          [json| {} |]
           `shouldRespondWith` ResponseMatcher {
              matchBody    = Just [str|"postgrest_test_default_role"|]
            , matchStatus = 200
            , matchHeaders = []
            }

      it "switches to postgrest_test_anonymous on id=3" $
        let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6M30.15Gy8PezQhJIaHYDJVLa-Gmz9T3sJnW66EKAYIsXc7c" in
        request methodPost "/rpc/get_current_user" [auth]
          [json| {} |]
           `shouldRespondWith` ResponseMatcher {
              matchBody    = Just [str|"postgrest_test_anonymous"|]
            , matchStatus = 200
            , matchHeaders = []
            }

      it "switches to postgrest_test_anonymous on no auth header" $
        request methodPost "/rpc/get_current_user" []
          [json| {} |]
           `shouldRespondWith` ResponseMatcher {
              matchBody    = Just [str|"postgrest_test_anonymous"|]
            , matchStatus = 200
            , matchHeaders = []
            }

      it "raises error on id=4" $
        let auth = authHeaderJWT "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6NH0.t_F7ULztyoyZYI1BkXa2AprkFZ6usbPF3LBwR6oT3po" in
        request methodPost "/rpc/get_current_user" [auth]
          [json| {} |]
           `shouldRespondWith` ResponseMatcher {
              matchBody    = Just [str|{"hint":"Please contact administrator","details":null,"code":"P0001","message":"Disabled ID --> 4"}|]
            , matchStatus = 400
            , matchHeaders = []
            }
