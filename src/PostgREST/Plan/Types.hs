module PostgREST.Plan.Types
  ( TypedField(..)
  , resolveTableField
  ) where

import qualified Data.HashMap.Strict.InsOrd as HMI

import PostgREST.SchemaCache.Identifiers (FieldName)
import PostgREST.SchemaCache.Table       (Column (..), Table (..))

import Protolude

-- | A TypedField is a field with sufficient information to be read from JSON with `json_to_recordset`.
data TypedField = TypedField
   { tfName    :: FieldName
   , tfIRType  :: Text -- ^ The initial type of the field, before any casting.
   , tfDefault :: Maybe Text
   } deriving (Eq)

resolveTableField :: Table -> FieldName -> Maybe TypedField
resolveTableField table fieldName =
  case HMI.lookup fieldName (tableColumns table) of
    Just column -> Just $ TypedField (colName column) (colNominalType column) (colDefault column)
    Nothing     -> Nothing
