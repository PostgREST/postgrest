{-# LANGUAGE CApiFFI #-}

-- |
-- Programmatic capture of server notices (RAISE WARNING et al).
--
-- Registers a libpq notice receiver that copies the structured diagnostic
-- fields off each notice's PGresult before libpq frees it. Notices accumulate
-- in a bounded per-connection buffer; sessions drain it after each command.
--
-- This bypasses postgresql-libpq's NoticeBuffer machinery entirely
-- ("enableNoticeReporting" / "getNotice"), which flattens every notice to its
-- rendered text and loses severity, SQLSTATE, detail and hint.
module Hasql.LibPq14.Notices
  ( Notice (..),
    NoticeChannel,
    newNoticeChannel,
    registerNoticeReceiver,
    destroyNoticeChannel,
    drainNotices,
    noticeChannelCapacity,
  )
where

import qualified Data.ByteString                    as BS
import           Data.IORef
import qualified Database.PostgreSQL.LibPQ          as LibPQ
import           Database.PostgreSQL.LibPQ.Internal (PGconn, withConn)
import           Foreign.C.String                   (CString)
import           Foreign.C.Types                    (CInt (..))
import           Foreign.Ptr                        (FunPtr, Ptr,
                                                     freeHaskellFunPtr, nullPtr)
import           Hasql.Prelude

-- |
-- A single non-fatal message received from the server.
--
-- Fields mirror the libpq @PG_DIAG_*@ diagnostics of the notice's PGresult.
data Notice = Notice
  { -- | e.g. @WARNING@ or @NOTICE@
    noticeSeverity :: BS.ByteString,
    -- | SQLSTATE code
    noticeCode     :: BS.ByteString,
    -- | Primary human-readable message
    noticeMessage  :: BS.ByteString,
    noticeDetail   :: Maybe BS.ByteString,
    noticeHint     :: Maybe BS.ByteString
  }
  deriving (Show, Eq)

-- | Maximum notices buffered per connection. Overflow drops the oldest,
-- bounding memory on connections that receive notices outside of sessions
-- (e.g. dedicated LISTEN connections that never drain).
noticeChannelCapacity :: Int
noticeChannelCapacity = 100

-- | Callback signature libpq expects: @void (*)(void *arg, const PGresult *res)@.
-- The result pointer is typed as @Ptr ()@ because postgresql-libpq's Internal
-- module does not export @PGresult@; the same approach is used by
-- "Hasql.LibPq14.Ffi" for its @PQresultStatus@ import.
type NoticeReceiverCb = Ptr () -> Ptr () -> IO ()

foreign import ccall "wrapper"
  mkNoticeReceiver :: NoticeReceiverCb -> IO (FunPtr NoticeReceiverCb)

foreign import capi "libpq-fe.h PQsetNoticeReceiver"
  pqSetNoticeReceiver :: Ptr PGconn -> FunPtr NoticeReceiverCb -> Ptr () -> IO (FunPtr NoticeReceiverCb)

foreign import capi "libpq-fe.h PQresultErrorField"
  pqResultErrorField :: Ptr () -> CInt -> IO CString

foreign import capi "postgres_ext.h value PG_DIAG_SEVERITY"        diagSeverityField :: CInt
foreign import capi "postgres_ext.h value PG_DIAG_SQLSTATE"        diagSqlstateField :: CInt
foreign import capi "postgres_ext.h value PG_DIAG_MESSAGE_PRIMARY" diagMessagePrimaryField :: CInt
foreign import capi "postgres_ext.h value PG_DIAG_MESSAGE_DETAIL"  diagMessageDetailField :: CInt
foreign import capi "postgres_ext.h value PG_DIAG_MESSAGE_HINT"    diagMessageHintField :: CInt

-- | Per-connection channel: the accumulating buffer plus the registered
-- receiver closure, so the 'FunPtr' can be freed on connection release.
data NoticeChannel = NoticeChannel
  { noticeChannelRef    :: !(IORef [Notice]),
    noticeChannelFunPtr :: !(FunPtr NoticeReceiverCb)
  }

-- | Allocate an empty channel with its receiver closure already wired to it.
newNoticeChannel :: IO NoticeChannel
newNoticeChannel = do
  ref <- newIORef []
  funPtr <- mkNoticeReceiver (\_ result -> receiveNotice ref result)
  pure (NoticeChannel ref funPtr)

-- | Install the channel's receiver on the given connection. The previously
-- installed receiver returned by libpq is dropped without freeing: if it was
-- libpq's default handler it is a static address, and freeing foreign static
-- function pointers is undefined behavior.
registerNoticeReceiver :: NoticeChannel -> LibPQ.Connection -> IO ()
registerNoticeReceiver channel connection =
  withConn connection $ \conn -> do
    _ <- pqSetNoticeReceiver conn (noticeChannelFunPtr channel) nullPtr
    pure ()

-- | Free the receiver closure. Must be called exactly once per channel,
-- after the connection using it is finished.
destroyNoticeChannel :: NoticeChannel -> IO ()
destroyNoticeChannel =
  freeHaskellFunPtr . noticeChannelFunPtr

-- | Remove and return all buffered notices, oldest first.
drainNotices :: NoticeChannel -> IO [Notice]
drainNotices channel =
  atomicModifyIORef' (noticeChannelRef channel) (\old -> ([], old))

-- | Receiver entry point: decode the PGresult's diagnostics and buffer them.
-- Runs inside libpq's input processing, while the calling session holds the
-- connection lock, so 'atomicModifyIORef'' suffices.
receiveNotice :: IORef [Notice] -> Ptr () -> IO ()
receiveNotice buffer result = do
  mNotice <- decodeNotice result
  traverse_ (appendNotice buffer) mNotice

decodeNotice :: Ptr () -> IO (Maybe Notice)
decodeNotice result = do
  mSeverity <- field diagSeverityField
  mCode <- field diagSqlstateField
  mMessage <- field diagMessagePrimaryField
  mDetail <- field diagMessageDetailField
  mHint <- field diagMessageHintField
  case (mSeverity, mCode, mMessage) of
    (Just severity, Just code, Just message) ->
      pure (Just (Notice severity code message mDetail mHint))
    _ ->
      pure Nothing
  where
    field ::
      CInt ->
      IO (Maybe BS.ByteString)
    field code = do
      cstr <- pqResultErrorField result code
      if cstr == nullPtr
        then pure Nothing
        else Just <$> BS.packCString cstr

appendNotice :: IORef [Notice] -> Notice -> IO ()
appendNotice buffer notice =
  atomicModifyIORef' buffer $ \old ->
    let grown = old ++ [notice]
        excess = max 0 (length grown - noticeChannelCapacity)
     in (drop excess grown, ())
