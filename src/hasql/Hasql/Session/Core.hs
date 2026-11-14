module Hasql.Session.Core where

import Hasql.Errors
import Hasql.Prelude

import Hasql.Connection.Core qualified as Connection
import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Result qualified as Decoders.Result
import Hasql.Decoders.Results qualified as Decoders.Results
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as Pq
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry
import Hasql.Statement qualified as Statement

-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a
  = Session (ReaderT Connection.Connection (ExceptT SessionError IO) a)
  deriving (Applicative, Functor, Monad, MonadError SessionError, MonadIO, MonadReader Connection.Connection)

-- |
-- Executes a bunch of commands on the provided connection.
run :: Session a -> Connection.Connection -> IO (Either SessionError a)
run (Session impl) connection =
  mask $ \restore -> onException (restore main) handler
  where
    main =
      runExceptT $ runReaderT impl connection
    handler =
      case connection of
        Connection.Connection _ pqConnVar _ registry _noticeChannel ->
          withMVar pqConnVar \pqConn -> do
            Pq.transactionStatus pqConn >>= \case
              Pq.TransIdle -> pure ()
              _ -> do
                PreparedStatementRegistry.reset registry
                Pq.reset pqConn

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql' =
  Session
    $ ReaderT
    $ \(Connection.Connection _ pqConnectionRef integerDatetimes _ noticeChannel) ->
      ExceptT
        $ withMVar pqConnectionRef
        $ \pqConnection -> do
          r1 <- IO.sendNonparametricStatement pqConnection sql'
          r2 <- IO.getResults pqConnection integerDatetimes decoder
          notices <- IO.drainNotices noticeChannel
          return $ case r1 *> r2 of
            Left commandError -> Left (QueryError sql' [] commandError notices)
            Right result -> Right result
  where
    decoder =
      Decoders.Results.single Decoders.Result.noResult

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session result
statement input (Statement.Statement template (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) =
  Session
    $ ReaderT
    $ \(Connection.Connection usePreparedStatements pqConnectionRef integerDatetimes registry noticeChannel) ->
      ExceptT
        $ withMVar pqConnectionRef
        $ \pqConnection -> do
          r1 <- IO.sendParametricStatement pqConnection integerDatetimes registry template paramsEncoder (usePreparedStatements && preparable) input
          r2 <- IO.getResults pqConnection integerDatetimes decoder
          notices <- IO.drainNotices noticeChannel
          return $ case r1 *> r2 of
            Left commandError -> Left (QueryError template (Encoders.Params.renderReadable paramsEncoder input) commandError notices)
            Right result -> Right result
