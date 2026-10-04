-- TODO: Move to doc-tests

spec :: Spec
spec = do
  describe "FatalError show instance" $
    it "extracts message" $
      ( show $
          FatalError{fatalErrorMessage = "some message"}
      )
        `shouldBe` "some message"
  describe "toPgIdenfier" $
    it "enclose text in quotes doubling existing ones" $
      fromPgIdentifier (toPgIdentifier "some \"identifier\"") `shouldBe` "\"some \"\"identifier\"\"\""
