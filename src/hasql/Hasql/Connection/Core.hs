-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection.Core where

import Hasql.Prelude

import Hasql.Connection.Config qualified as Config
import Hasql.Connection.Setting qualified as Setting
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as LibPQ
import Hasql.LibPq14.Notices qualified as Notices
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry

-- |
-- A single connection to the database.
data Connection
  = Connection
      !Bool
      -- ^ Whether prepared statements are allowed.
      !(MVar LibPQ.Connection)
      -- ^ Lower level libpq connection.
      !Bool
      -- ^ Integer datetimes.
      !PreparedStatementRegistry.PreparedStatementRegistry
      -- ^ Prepared statement registry.
      !Notices.NoticeChannel
      -- ^ Buffer of notices received from the server during command execution.

-- |
-- Possible details of the connection acquistion error.
type ConnectionError =
  Maybe ByteString

-- |
-- Establish a connection according to the provided settings.
acquire
  :: [Setting.Setting]
  -> IO (Either ConnectionError Connection)
acquire settings =
  {-# SCC "acquire" #-}
  runExceptT $ do
    pqConnection <- lift (IO.acquireConnection (Config.connectionString config))
    lift (IO.checkConnectionStatus pqConnection) >>= traverse_ throwError
    integerDatetimes <- lift (IO.getIntegerDatetimes pqConnection)
    registry <- lift IO.acquirePreparedStatementRegistry
    pqConnectionRef <- lift (newMVar pqConnection)
    -- The channel is created last so that only 'initConnection' can fail
    -- after its FunPtr exists; that one call is guarded below.
    noticeChannel <- lift Notices.newNoticeChannel
    lift
      ( IO.initConnection pqConnection noticeChannel
          `onException` do
            -- Finish libpq first so its receiver can no longer fire, then
            -- free the closure; also covers the underlying connection itself.
            IO.releaseConnection pqConnection
            Notices.destroyNoticeChannel noticeChannel
      )
    pure (Connection (Config.usePreparedStatements config) pqConnectionRef integerDatetimes registry noticeChannel)
  where
    config = Config.fromUpdates settings

-- |
-- Release the connection.
release :: Connection -> IO ()
release (Connection _ pqConnectionRef _ _ noticeChannel) =
  mask_ $ do
    nullConnection <- LibPQ.newNullConnection
    pqConnection <- swapMVar pqConnectionRef nullConnection
    IO.releaseConnection pqConnection
    Notices.destroyNoticeChannel noticeChannel

-- |
-- Execute an operation on the raw @libpq@ 'LibPQ.Connection'.
--
-- The access to the connection is exclusive.
withLibPQConnection :: Connection -> (LibPQ.Connection -> IO a) -> IO a
withLibPQConnection (Connection _ pqConnectionRef _ _ _) =
  withMVar pqConnectionRef
