{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Unit.ConfigSpec where {
  import Test.Hspec;
  import Config(AppConfig(..), argParser);

spec :: Spec;
spec = describe "argParser" $ do
  it "parses all valid options" $
    argParser [
      "--db-name", "postresty",
      "--db-port", "1337",
      "--db-user", "Basscadet",
      "--db-pass", "what?",
      "--db-host", "ample.co",
      "--port", "31337",
      "--anonymous", "anon",
      "--secure",
      "--db-pool", "123"] `shouldBe` Right (AppConfig
      "postresty"
      1337
      "Basscadet"
      "what?"
      "ample.co"
      31337
      "anon"
      True
      123)

  it "has sane defaults" $
    argParser [ "-d", "postgresty", "-a", "anon", "-U", "Autechre"]
    `shouldBe` Right (AppConfig
      "postgresty" 5432 "Autechre" "" "localhost" 3000 "anon" False 10)

  it "requires database, anonymous, and user" $ do
    let Left errs = argParser ["-d", "postgresty", "-a", "anon"]
    length errs `shouldBe` 1
    let Left errs = argParser ["-U", "postgresty", "-a", "anon"]
    length errs `shouldBe` 1
    let Left errs = argParser ["-d", "postgresty", "-U", "anon"]
    length errs `shouldBe` 1

  it "finds multiple errors at once" $ do
    let Left errs = argParser ["-d", "postgresty"]
    length errs `shouldBe` 2
    let Left errs = argParser []
    length errs `shouldBe` 3
}
