module Hasql.LibPq14.Mappings where

#include "libpq-fe.h"

import Foreign.C.Types (CInt (..))
import Hasql.Prelude

data ExecStatus
  = EmptyQuery
  | CommandOk
  | TuplesOk
  | CopyOut
  | CopyIn
  | CopyBoth
  | BadResponse
  | NonfatalError
  | FatalError
  | SingleTuple
  deriving (Eq, Show)

decodeExecStatus :: CInt -> Maybe ExecStatus
decodeExecStatus = \case
  (#const PGRES_EMPTY_QUERY) -> Just EmptyQuery
  (#const PGRES_COMMAND_OK) -> Just CommandOk
  (#const PGRES_TUPLES_OK) -> Just TuplesOk
  (#const PGRES_COPY_OUT) -> Just CopyOut
  (#const PGRES_COPY_IN) -> Just CopyIn
  (#const PGRES_COPY_BOTH) -> Just CopyBoth
  (#const PGRES_BAD_RESPONSE) -> Just BadResponse
  (#const PGRES_NONFATAL_ERROR) -> Just NonfatalError
  (#const PGRES_FATAL_ERROR) -> Just FatalError
  (#const PGRES_SINGLE_TUPLE) -> Just SingleTuple
  _ -> Nothing
