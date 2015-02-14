module Unit.ConfigSpec where
import Test.Hspec
import Config(argParser)
import Control.Monad(join)
import Record(r)
import Data.List(isInfixOf)
import Data.Either(lefts)

errorList :: (Either [a] b) -> [a]
errorList e = join $ lefts [e]

errorCount :: (Either [a] b) -> Int
errorCount = length . errorList

spec :: Spec
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
    errorCount (argParser ["-d", "postgresty", "-a", "anon"]) `shouldBe` 1
    errorCount (argParser ["-U", "postgresty", "-a", "anon"]) `shouldBe` 1
    errorCount (argParser ["-d", "postgresty", "-U", "anon"]) `shouldBe` 1

  it "finds multiple missing args at once" $ do
    errorList (argParser ["-d", "postgresty"])
      `shouldSatisfy` \[e] -> isInfixOf "anonymous" e && isInfixOf "user" e

    errorList (argParser []) `shouldSatisfy`
      \[e] -> isInfixOf "anonymous" e && isInfixOf "user" e && isInfixOf "name" e

  it "emits errors for misparsed args" $
    errorCount (argParser ["-d", "pg", "-a", "anon", "-u", "pg", "-p", "nan"])
      `shouldBe` 1

  it "finds all these errors" $
    errorCount (argParser ["-d", "pg", "-p", "eighty", "--db-pool", "ten"])
      `shouldBe` 3
