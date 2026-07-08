module Hasql.Transaction.Private.Statements where

import qualified Hasql.Decoders                    as C
import qualified Hasql.Encoders                    as B
import qualified Hasql.Statement                   as A
import           Hasql.Transaction.Config
import           Hasql.Transaction.Private.Prelude
import qualified Hasql.Transaction.Private.SQL     as D

beginTransaction :: IsolationLevel -> Mode -> A.Statement () ()
beginTransaction isolation mode =
  A.Statement (D.beginTransaction isolation mode) B.noParams C.noResult True

commitTransaction :: A.Statement () ()
commitTransaction =
  A.Statement "COMMIT" B.noParams C.noResult True

abortTransaction :: A.Statement () ()
abortTransaction =
  A.Statement "ABORT" B.noParams C.noResult True
