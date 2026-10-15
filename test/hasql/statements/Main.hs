module Main where

import Contravariant.Extras (contrazip2)
import Control.Monad.Fail (fail)
import Protolude
import Test.Hspec
import Test.QuickCheck (Property, ioProperty, property, (===), (==>))

import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

main :: IO ()
main = hspec $ do
  describe "Roundtrips" $ do
    it "Array" $
      let
        encoder = Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int4)))))
        decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int4))))))
      in
        property $ roundtrip encoder decoder

    it "2D Array" $
      let
        encoder = Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int4))))))
        decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int4)))))))
      in
        property $ \input -> input /= [] ==> roundtrip encoder decoder (replicate 3 input)

  around withConnection $ describe "Prepared statements" $ do
    it "Failed query" $ \connection -> do
      let
        encoder =
          contrazip2
            (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int4))))))
            (Encoders.param (Encoders.nonNullable Encoders.text))
        decoder = isJust <$> Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))
        statement = Statement.Statement "select true where 1 = any ($1) and $2" encoder decoder True
      result <- Session.run (Session.statement ([3, 7], "a") statement) connection
      result `shouldSatisfy` \case
        Left (Session.QueryError "select true where 1 = any ($1) and $2" ["[3, 7]", "\"a\""] _) -> True
        _ -> False

    it "Failing prepared statements" $ \connection -> do
      let
        statement = Statement.Statement "absurd" mempty Decoders.noResult True
        session = Session.statement () statement
      result <- Session.run (catchError session (const (pure ())) *> session) connection
      result `shouldSatisfy` \case
        Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "26000" _ _ _))) -> False
        _ -> True

    it "Prepared statements after error" $ \connection -> do
      let
        statement = Statement.Statement "select $1 :: int4" int4Encoder int4Decoder True
        session = do
          _ <- Session.statement 1 statement
          catchError (Session.sql "absurd") (const (pure ()))
          Session.statement 1 statement
      result <- Session.run session connection
      result `shouldSatisfy` isRight

    it "\"in progress after error\" bugfix" $ \connection -> do
      let session = Session.sql "begin" *> Session.statement (1, 1) sumStatement <* Session.sql "end"
      _ <- Session.run (Session.sql "asldfjsldk") connection
      result <- Session.run session connection
      result `shouldSatisfy` isRight

    it "\"another command is already in progress\" bugfix" $ \connection -> do
      let session = do
            Session.sql "begin;"
            result <- Session.statement (1, 1) sumStatement
            Session.sql "end;"
            pure result
      Session.run session connection `shouldReturn` Right 2

    it "The same prepared statement used on different types" $ \connection -> do
      let
        textStatement =
          Statement.Statement
            "select $1"
            (Encoders.param (Encoders.nonNullable Encoders.text))
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
            True
        int4Statement = Statement.Statement "select $1" int4Encoder int4Decoder True
        session = (,) <$> Session.statement "ok" textStatement <*> Session.statement 1 int4Statement
      Session.run session connection `shouldReturn` Right ("ok", 1)

withConnection :: (Connection.Connection -> IO a) -> IO a
withConnection = bracket acquire Connection.release
  where
    -- libpq reads the connection parameters from the PG* variables.
    acquire = Connection.acquire [Setting.connection ""] >>= either (fail . show) pure

roundtrip :: (Eq a, Show a) => Encoders.Params a -> Decoders.Result a -> a -> Property
roundtrip encoder decoder input = ioProperty $ withConnection $ \connection -> do
  let statement = Statement.Statement "select $1" encoder decoder True
  result <- Session.run (Session.statement input statement) connection
  pure (result === Right input)

int4Encoder :: Encoders.Params Int32
int4Encoder = Encoders.param (Encoders.nonNullable Encoders.int4)

int4Decoder :: Decoders.Result Int32
int4Decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4))

sumStatement :: Statement.Statement (Int32, Int32) Int32
sumStatement =
  Statement.Statement
    "select ($1 + $2)"
    (contrazip2 int4Encoder int4Encoder)
    int4Decoder
    True
