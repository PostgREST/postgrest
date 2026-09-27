module Hasql.LibPq14
  ( module Base

    -- * Updated and new types
  , Mappings.ExecStatus (..)

    -- * Updated and new procedures
  , resultStatus
  )
where

import Database.PostgreSQL.LibPQ as Base hiding
  ( ExecStatus (..)
  , PipelineStatus (..)
  , enterPipelineMode
  , exitPipelineMode
  , pipelineSync
  , resultStatus
  , sendFlushRequest
  )

import Hasql.Prelude

import Hasql.LibPq14.Ffi qualified as Ffi
import Hasql.LibPq14.Mappings qualified as Mappings

resultStatus :: Result -> IO Mappings.ExecStatus
resultStatus result = do
  -- Unsafe-coercing because the constructor is not exposed by the lib,
  -- but it's implemented as a newtype over ForeignPtr.
  -- Since internal changes in the \"postgresql-lipbq\" may break this,
  -- it requires us to avoid using an open dependency range on it.
  ffiStatus <- withForeignPtr (unsafeCoerce result) Ffi.resultStatus
  decodeProcedureResult "resultStatus" Mappings.decodeExecStatus ffiStatus

decodeProcedureResult
  :: (Show a)
  => String
  -> (a -> Maybe b)
  -> a
  -> IO b
decodeProcedureResult label decoder ffiResult =
  case decoder ffiResult of
    Just res -> pure res
    Nothing -> fail ("Failed to decode result of " <> label <> " from: " <> show ffiResult)
