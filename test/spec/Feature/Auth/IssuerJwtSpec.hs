module Feature.Auth.IssuerJwtSpec where

import Network.HTTP.Types
import Protolude hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config (AppConfig (..), parseSecret)
import SpecHelper

spec :: SpecWithConfig
spec withConfig = withConfig
  ( baseCfg
      { configJwtSecret = Just generateSecret,
        configJwtIssuer = Just "https://issuer.example.com",
        configJWKS = rightToMaybe $ parseSecret generateSecret
      }
  )
  $ describe "test handling of iss claims in JWT when the jwt-iss config is set"
  $ do
    -- this test will stop working 9999999999s after the UNIX EPOCH
    it "succeeds when the issuer claim matches" $ do
      let
        jwtPayload =
          [json|{
            "exp": 9999999999,
            "role": "postgrest_test_author",
            "iss": "https://issuer.example.com"
          }|]
        auth = authHeaderJWT $ generateJWT jwtPayload
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` 200

    it "fails when the issuer claim does not match" $ do
      let
        jwtPayload =
          [json|{
            "exp": 9999999999,
            "role": "postgrest_test_author",
            "iss": "https://other.example.com"
          }|]
        auth = authHeaderJWT $ generateJWT jwtPayload
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` [json|{"code":"PGRST303","details":null,"hint":null,"message":"JWT issuer does not match"}|]
          { matchStatus = 401
          }

    it "fails when the issuer claim only differs by a trailing slash" $ do
      let
        jwtPayload =
          [json|{
            "exp": 9999999999,
            "role": "postgrest_test_author",
            "iss": "https://issuer.example.com/"
          }|]
        auth = authHeaderJWT $ generateJWT jwtPayload
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` [json|{"code":"PGRST303","details":null,"hint":null,"message":"JWT issuer does not match"}|]
          { matchStatus = 401
          }

    it "fails when the issuer claim is missing" $ do
      let
        jwtPayload =
          [json|{
            "exp": 9999999999,
            "role": "postgrest_test_author"
          }|]
        auth = authHeaderJWT $ generateJWT jwtPayload
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` [json|{"code":"PGRST303","details":null,"hint":null,"message":"JWT issuer does not match"}|]
          { matchStatus = 401
          }

    it "fails when the issuer claim is not a string" $ do
      let
        jwtPayload =
          [json|{
            "exp": 9999999999,
            "role": "postgrest_test_author",
            "iss": 1
          }|]
        auth = authHeaderJWT $ generateJWT jwtPayload
      request methodGet "/authors_only" [auth] ""
        `shouldRespondWith` [json|{"code":"PGRST303","details":null,"hint":null,"message":"The JWT 'iss' claim must be a string"}|]
          { matchStatus = 401
          }

    it "succeeds without a JWT" $
      get "/has_count_column" `shouldRespondWith` 200

disabledSpec :: SpecWithConfig
disabledSpec withConfig = withConfig baseCfg $ describe "test handling of iss claims in JWT when the jwt-iss config is not set" $ do
  it "ignores the issuer claim" $ do
    let
      jwtPayload =
        [json|{
          "exp": 9999999999,
          "role": "postgrest_test_author",
          "iss": "https://issuer.example.com"
        }|]
      auth = authHeaderJWT $ generateJWT jwtPayload
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200

  it "ignores an issuer claim that is not a string" $ do
    let
      jwtPayload =
        [json|{
          "exp": 9999999999,
          "role": "postgrest_test_author",
          "iss": 1
        }|]
      auth = authHeaderJWT $ generateJWT jwtPayload
    request methodGet "/authors_only" [auth] ""
      `shouldRespondWith` 200
