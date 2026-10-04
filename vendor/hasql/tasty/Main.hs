module Main where

import Contravariant.Extras
import Main.Prelude hiding (assert)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners

import Hasql.TestingKit.TestingDsl qualified as Session
import Main.Connection qualified as Connection
import Main.Statements qualified as Statements

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

main :: IO ()
main =
  defaultMain tree

tree :: TestTree
tree =
  localOption (NumThreads 1) $
    testGroup
      "All tests"
      [ testGroup "Roundtrips" $
          let roundtrip encoder decoder input =
                let session =
                      let statement = Statement.Statement "select $1" encoder decoder True
                      in  Session.statement input statement
                in  unsafePerformIO $ do
                      x <- Connection.with (Session.run session)
                      return (Right (Right input) === x)
          in  [ testProperty "Array" $
                  let
                    encoder = Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8)))))
                    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int8))))))
                  in
                    roundtrip encoder decoder
              , testProperty "2D Array" $
                  let
                    encoder = Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int8)))))))
                  in
                    \list -> list /= [] ==> roundtrip encoder decoder (replicate 3 list)
              ]
      , testCase "Failed query" $
          let
            statement =
              Statement.Statement "select true where 1 = any ($1) and $2" encoder decoder True
              where
                encoder =
                  contrazip2
                    (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                    (Encoders.param (Encoders.nonNullable Encoders.text))
                decoder =
                  fmap Data.Maybe.isJust (Decoders.rowMaybe ((Decoders.column . Decoders.nonNullable) Decoders.bool))
            session =
              Session.statement ([3, 7], "a") statement
          in
            do
              x <- Connection.with (Session.run session)
              assertBool (show x) $ case x of
                Right (Left (Session.QueryError "select true where 1 = any ($1) and $2" ["[3, 7]", "\"a\""] _)) -> True
                _ -> False
      , testCase "Failing prepared statements" $
          let io =
                Connection.with (Session.run session)
                  >>= (assertBool <$> show <*> resultTest)
                where
                  resultTest =
                    \case
                      Right (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "26000" _ _ _)))) -> False
                      _ -> True
                  session =
                    catchError session (const (pure ())) *> session
                    where
                      session =
                        Session.statement () statement
                        where
                          statement =
                            Statement.Statement sql encoder decoder True
                            where
                              sql =
                                "absurd"
                              encoder =
                                mempty
                              decoder =
                                Decoders.noResult
          in  io
      , testCase "Prepared statements after error" $
          let io =
                Connection.with (Session.run session)
                  >>= \x -> assertBool (show x) (either (const False) isRight x)
                where
                  session =
                    try *> fail *> try
                    where
                      try =
                        Session.statement 1 statement
                        where
                          statement =
                            Statement.Statement sql encoder decoder True
                            where
                              sql =
                                "select $1 :: int8"
                              encoder =
                                Encoders.param (Encoders.nonNullable Encoders.int8)
                              decoder =
                                Decoders.singleRow $ (Decoders.column . Decoders.nonNullable) Decoders.int8
                      fail =
                        catchError (Session.sql "absurd") (const (pure ()))
          in  io
      , testCase "\"in progress after error\" bugfix" $
          let
            sumStatement :: Statement.Statement (Int64, Int64) Int64
            sumStatement =
              Statement.Statement sql encoder decoder True
              where
                sql =
                  "select ($1 + $2)"
                encoder =
                  contramap fst (Encoders.param (Encoders.nonNullable Encoders.int8))
                    <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.int8))
                decoder =
                  Decoders.singleRow ((Decoders.column . Decoders.nonNullable) Decoders.int8)
            sumSession :: Session.Session Int64
            sumSession =
              Session.sql "begin" *> Session.statement (1, 1) sumStatement <* Session.sql "end"
            errorSession :: Session.Session ()
            errorSession =
              Session.sql "asldfjsldk"
            io =
              Connection.with $ \c -> do
                _ <- Session.run errorSession c
                Session.run sumSession c
          in
            io >>= \x -> assertBool (show x) (either (const False) isRight x)
      , testCase "\"another command is already in progress\" bugfix" $
          let
            sumStatement :: Statement.Statement (Int64, Int64) Int64
            sumStatement =
              Statement.Statement sql encoder decoder True
              where
                sql =
                  "select ($1 + $2)"
                encoder =
                  contramap fst (Encoders.param (Encoders.nonNullable Encoders.int8))
                    <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.int8))
                decoder =
                  Decoders.singleRow ((Decoders.column . Decoders.nonNullable) Decoders.int8)
            session :: Session.Session Int64
            session =
              do
                Session.sql "begin;"
                s <- Session.statement (1, 1) sumStatement
                Session.sql "end;"
                return s
          in
            Session.runSessionOnLocalDb session >>= \x -> assertEqual (show x) (Right 2) x
      , testCase "The same prepared statement used on different types" $
          let actualIO =
                Session.runSessionOnLocalDb $ do
                  let
                    effect1 =
                      Session.statement "ok" statement
                      where
                        statement =
                          Statement.Statement sql encoder decoder True
                          where
                            sql =
                              "select $1"
                            encoder =
                              Encoders.param (Encoders.nonNullable Encoders.text)
                            decoder =
                              Decoders.singleRow ((Decoders.column . Decoders.nonNullable) Decoders.text)
                    effect2 =
                      Session.statement 1 statement
                      where
                        statement =
                          Statement.Statement sql encoder decoder True
                          where
                            sql =
                              "select $1"
                            encoder =
                              Encoders.param (Encoders.nonNullable Encoders.int8)
                            decoder =
                              Decoders.singleRow ((Decoders.column . Decoders.nonNullable) Decoders.int8)
                   in
                    (,) <$> effect1 <*> effect2
          in  actualIO >>= assertEqual "" (Right ("ok", 1))
      , testCase "Affected rows counting" $
          replicateM_ 13 $
            let actualIO =
                  Session.runSessionOnLocalDb $ do
                    dropTable
                    createTable
                    replicateM_ 100 insertRow
                    deleteRows <* dropTable
                  where
                    dropTable =
                      Session.statement () $
                        Statements.plain "drop table if exists a"
                    createTable =
                      Session.statement () $
                        Statements.plain "create table a (id bigserial not null, name varchar not null, primary key (id))"
                    insertRow =
                      Session.statement () $
                        Statements.plain "insert into a (name) values ('a')"
                    deleteRows =
                      Session.statement () $ Statement.Statement sql mempty decoder False
                      where
                        sql =
                          "delete from a"
                        decoder =
                          Decoders.rowsAffected
            in  actualIO >>= assertEqual "" (Right 100)
      ]
