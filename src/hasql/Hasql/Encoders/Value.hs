module Hasql.Encoders.Value where

import qualified Hasql.PostgresTypeInfo     as PTI
import           Hasql.Prelude
import qualified PostgreSQL.Binary.Encoding as B
import qualified TextBuilder                as C

data Value a
  = Value PTI.OID PTI.OID (Bool -> a -> B.Encoding) (a -> C.TextBuilder)

instance Contravariant Value where
  {-# INLINE contramap #-}
  contramap f (Value valueOID arrayOID encode render) =
    Value valueOID arrayOID (\integerDatetimes input -> encode integerDatetimes (f input)) (render . f)

{-# INLINE unsafePTI #-}
unsafePTI :: PTI.PTI -> (Bool -> a -> B.Encoding) -> (a -> C.TextBuilder) -> Value a
unsafePTI pti =
  Value (PTI.ptiOID pti) (fromMaybe (error "No array OID") (PTI.ptiArrayOID pti))

{-# INLINE unsafePTIWithShow #-}
unsafePTIWithShow :: (Show a) => PTI.PTI -> (Bool -> a -> B.Encoding) -> Value a
unsafePTIWithShow pti encode =
  unsafePTI pti encode (C.string . show)
