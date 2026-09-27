module Hasql.PostgresTypeInfo where

import Hasql.Prelude

import Hasql.LibPq14 qualified as LibPQ

-- | A Postgresql type info
data PTI = PTI {ptiOID :: !OID, ptiArrayOID :: !(Maybe OID)}

-- | A Word32 and a LibPQ representation of an OID
data OID = OID {oidWord32 :: !Word32, oidPQ :: !LibPQ.Oid, oidFormat :: !LibPQ.Format}

mkOID :: LibPQ.Format -> Word32 -> OID
mkOID format x =
  OID x ((LibPQ.Oid . fromIntegral) x) format

mkPTI :: LibPQ.Format -> Word32 -> Maybe Word32 -> PTI
mkPTI format oid' arrayOID =
  PTI (mkOID format oid') (fmap (mkOID format) arrayOID)

-- * Constants

bytea :: PTI
bytea = mkPTI LibPQ.Binary 17 (Just 1001)

int4 :: PTI
int4 = mkPTI LibPQ.Binary 23 (Just 1007)

json :: PTI
json = mkPTI LibPQ.Binary 114 (Just 199)

jsonb :: PTI
jsonb = mkPTI LibPQ.Binary 3802 (Just 3807)

text :: PTI
text = mkPTI LibPQ.Binary 25 (Just 1009)

textUnknown :: PTI
textUnknown = mkPTI LibPQ.Text 705 (Just 705)
