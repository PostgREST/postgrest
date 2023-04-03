{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.Plan.CallPlan
  ( CallPlan(..)
  , CallParams(..)
  , jsonRpcParams
  )
where

import qualified Data.Aeson                        as JSON
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.HashMap.Strict               as HM
import           PostgREST.SchemaCache.Identifiers (FieldName,
                                                    QualifiedIdentifier)
import           PostgREST.SchemaCache.Proc        (ProcDescription (..),
                                                    ProcParam (..))

import Protolude

data CallPlan = FunctionCall
  { funCQi           :: QualifiedIdentifier
  , funCParams       :: CallParams
  , funCArgs         :: Maybe LBS.ByteString
  , funCScalar       :: Bool
  , funCReturning    :: [FieldName]
  }

data CallParams
  = KeyParams [ProcParam] -- ^ Call with key params: func(a := val1, b:= val2)
  | OnePosParam ProcParam -- ^ Call with positional params(only one supported): func(val)

-- | Convert rpc params `/rpc/func?a=val1&b=val2` to json `{"a": "val1", "b": "val2"}
jsonRpcParams :: ProcDescription -> [(Text, Text)] -> LBS.ByteString
jsonRpcParams proc prms =
  if not $ pdHasVariadic proc then -- if proc has no variadic param, save steps and directly convert to json
    JSON.encode $ HM.fromList $ second JSON.toJSON <$> prms
  else
    let paramsMap = HM.fromListWith mergeParams $ toRpcParamValue proc <$> prms in
    JSON.encode paramsMap
  where
    mergeParams :: RpcParamValue -> RpcParamValue -> RpcParamValue
    mergeParams (Variadic a) (Variadic b) = Variadic $ b ++ a
    mergeParams v _                       = v -- repeated params for non-variadic parameters are not merged

toRpcParamValue :: ProcDescription -> (Text, Text) -> (Text, RpcParamValue)
toRpcParamValue proc (k, v) | prmIsVariadic k = (k, Variadic [v])
                            | otherwise       = (k, Fixed v)
  where
    prmIsVariadic prm = isJust $ find (\ProcParam{ppName, ppVar} -> ppName == prm && ppVar) $ pdParams proc

-- | RPC query param value `/rpc/func?v=<value>`, used for VARIADIC functions on form-urlencoded POST and GETs
-- | It can be fixed `?v=1` or repeated `?v=1&v=2&v=3.
data RpcParamValue = Fixed Text | Variadic [Text]
instance JSON.ToJSON RpcParamValue where
  toJSON (Fixed    v) = JSON.toJSON v
  toJSON (Variadic v) = JSON.toJSON v
