{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.SchemaCache.Identifiers
  ( QualifiedIdentifier(..)
  , Schema
  , TableName
  , FieldName
  , AccessSet
  , dumpQi
  , toQi
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Set   as S
import qualified Data.Text  as T

import Protolude


-- | Represents a pg identifier with a prepended schema name "schema.table".
-- When qiSchema is "", the schema is defined by the pg search_path.
data QualifiedIdentifier = QualifiedIdentifier
  { qiSchema :: Schema
  , qiName   :: TableName
  }
  deriving (Eq, Ord, Generic, JSON.ToJSON, JSON.ToJSONKey)

instance Hashable QualifiedIdentifier

dumpQi :: QualifiedIdentifier -> Text
dumpQi (QualifiedIdentifier s i) =
  (if T.null s then mempty else s <> ".") <> i

-- TODO: Handle a case where the QI comes like this: "my.fav.schema"."my.identifier"
-- Right now it only handles the schema.identifier case
toQi :: Text -> QualifiedIdentifier
toQi txt = case T.drop 1 <$> T.breakOn "." txt of
  (i, "") -> QualifiedIdentifier mempty i
  (s, i)  -> QualifiedIdentifier s i

type Schema = Text
type TableName = Text
type FieldName = Text

type AccessSet = S.Set QualifiedIdentifier
