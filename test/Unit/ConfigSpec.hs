{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE QuasiQuotes #-}
module Unit.ConfigSpec where {
  --indent for eazy folding.
  import Test.Hspec;
  import Config(argParser);
  import Record(r);

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
      "--db-pool", "123"] `shouldBe` Right [r|{
      dbName = "postresty",
      dbPort = 1337,
      dbUser = "Basscadet",
      dbPass = "what?",
      dbHost = "ample.co",
      port = 31337,
      anonRole = "anon",
      secure = True,
      pool = 123 }|]

  it "has sane defaults" $
    argParser [ "-d", "postgresty", "-a", "anon", "-U", "Autechre"]
    `shouldBe` Right [r|{dbName="postgresty", dbPort=5432, dbUser="Autechre",
      dbPass="", dbHost="localhost", port=3000, anonRole="anon",
      secure=False, pool=10}|]

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
