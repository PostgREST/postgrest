module Unit.ErrorsSpec where

import Test.Hspec

import Text.Parsec
import PgError
import Data.Either (rights)

spec :: Spec
spec =
  describe "Parsing Hasql errors" $ do
    it "can handle status and code" $
      let p = parse message "" "Status: \"foo\"; Code: \"abc\"." in
      rights [p] `shouldBe` [
          Message (Just "foo") "abc" Nothing Nothing
        ]
    it "can handle weird redundant quotes in status" $
      let p = parse message "" "Status: \"\"foo\"\"; Code: \"abc\"." in
      rights [p] `shouldBe` [
          Message (Just "foo") "abc" Nothing Nothing
        ]
    it "can handle text and code" $
      let p = parse message "" "Message: \"foo\"; Code: \"abc\"." in
      rights [p] `shouldBe` [
          Message Nothing "abc" (Just "foo") Nothing
        ]
    it "can handle status, text and code" $
      let p = parse message "" "Status: \"hi\"; Message: \"foo\"; Code: \"abc\"." in
      rights [p] `shouldBe` [
          Message (Just "hi") "abc" (Just "foo") Nothing
        ]
    it "can handle unescaped quotes in message" $
      let p = parse message "" "Status: \"hi\"; Message: \"unknown \"foo\"!\"; Code: \"abc\"." in
      rights [p] `shouldBe` [
          Message (Just "hi") "abc" (Just "unknown \"foo\"!") Nothing
        ]
    it "can handle periods in message" $
      let p = parse message "" "Message: \"unknown \"foo\".bar\"; Code: \"42P01\"." in
      rights [p] `shouldBe` [
          Message Nothing "42P01" (Just "unknown \"foo\".bar") Nothing
        ]
