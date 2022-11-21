module PostgREST.Plan.Types
  ( TypedField(..)
  , resolveField
  , resolveTableField

  ) where

import qualified Data.HashMap.Strict.InsOrd as HMI

import PostgREST.ApiRequest.Types (Field, JsonPath)

import PostgREST.SchemaCache.Identifiers (FieldName)
import PostgREST.SchemaCache.Table       (Column (..), Table (..))

import Protolude

-- | A TypedField is a field with sufficient information to be read from JSON with `json_to_recordset`.
data TypedField = TypedField
   { tfFieldName :: FieldName
   , tfJsonPath  :: JsonPath
   , tfIRType    :: Text
   } deriving (Eq)

resolveField :: Field -> Text -> TypedField
resolveField (fieldName, jsonPath) = TypedField fieldName jsonPath

resolveTableField :: Table -> FieldName -> Maybe TypedField
resolveTableField table fieldName =
  case HMI.lookup fieldName (tableColumns table) of
    Just column -> Just $ resolveField (colName column, []) (colNominalType column)
    Nothing     -> Nothing
