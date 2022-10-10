module PostgREST.Plan.CallPlan
  ( CallPlan(..)
  , CallParams(..)
  )
where

import qualified Data.ByteString.Lazy              as LBS
import           PostgREST.SchemaCache.Identifiers (FieldName,
                                                    QualifiedIdentifier)
import           PostgREST.SchemaCache.Proc        (ProcParam (..))

import Protolude

data CallPlan = FunctionCall
  { funCQi           :: QualifiedIdentifier
  , funCParams       :: CallParams
  , funCArgs         :: Maybe LBS.ByteString
  , funCScalar       :: Bool
  , funCMultipleCall :: Bool
  , funCReturning    :: [FieldName]
  }

data CallParams
  = KeyParams [ProcParam] -- ^ Call with key params: func(a := val1, b:= val2)
  | OnePosParam ProcParam -- ^ Call with positional params(only one supported): func(val)
