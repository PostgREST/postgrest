module PostgREST.Plan.Types
  ( CoercibleField(..)
  , unknownField
  , CoercibleLogicTree(..)
  , CoercibleFilter(..)
  , TransformerProc
  ) where

import PostgREST.ApiRequest.Types (JsonPath, LogicOperator, OpExpr)

import PostgREST.SchemaCache.Identifiers (FieldName)

import Protolude

type TransformerProc = Text

-- | A CoercibleField pairs the name of a query element with any type coercion information we need for some specific use case.
-- |
-- | As suggested by the name, it's often a reference to a field in a table but really it can be any nameable element (function parameter, calculation with an alias, etc) with a knowable type.
-- |
-- | In the simplest case, it allows us to parse JSON payloads with `json_to_recordset`, for which we need to know both the name and the type of each thing we'd like to extract. At a higher level, CoercibleField generalises to reflect that any value we work with in a query may need type specific handling.
-- |
-- | CoercibleField is the foundation for the Data Representations feature. This feature allow user-definable mappings between database types so that the same data can be presented or interpreted in various ways as needed. Sometimes the way Postgres coerces data implicitly isn't right for the job. Different mappings might be appropriate for different situations: parsing a filter from a query string requires one function (text -> field type) while parsing a payload from JSON takes another (json -> field type). And the reverse, outputting a field as JSON, requires yet a third (field type -> json). CoercibleField is that "job specific" reference to an element paired with the type we desire for that particular purpose and the function we'll use to get there, if any.
-- |
-- | In the planning phase, we "resolve" generic named elements into these specialised CoercibleFields. Again this is context specific: two different CoercibleFields both representing the exact same table column in the database, even in the same query, might have two different target types and mapping functions. For example, one might represent a column in a filter, and another the very same column in an output role to be sent in the response body.
-- |
-- | The type value is allowed to be the empty string. The analog here is soft type checking in programming languages: sometimes we don't need a variable to have a specified type and things will work anyhow. So the empty type variant is valid when we don't know and *don't need to know* about the specific type in some context. Note that this variation should not be used if it guarantees failure: in that case you should instead raise an error at the planning stage and bail out. For example, we can't parse JSON with `json_to_recordset` without knowing the types of each recipient field, and so error out. Using the empty string for the type would be incorrect and futile. On the other hand we use the empty type for RPC calls since type resolution isn't implemented for RPC, but it's fine because the query still works with Postgres' implicit coercion. In the future, hopefully we will support data representations across the board and then the empty type may be permanently retired.
data CoercibleField = CoercibleField
  { cfName      :: FieldName
  , cfJsonPath  :: JsonPath
  , cfIRType    :: Text -- ^ The native Postgres type of the field, the type before mapping.
  , cfTransform :: Maybe TransformerProc -- ^ The optional mapping from irType -> targetType.
  } deriving (Eq)

unknownField :: FieldName -> JsonPath -> CoercibleField
unknownField name path = CoercibleField name path "" Nothing

-- | Like an API request LogicTree, but with coercible field information.
data CoercibleLogicTree
  = CoercibleExpr Bool LogicOperator [CoercibleLogicTree]
  | CoercibleStmnt CoercibleFilter
  deriving (Eq)

data CoercibleFilter = CoercibleFilter
  { field  :: CoercibleField
  , opExpr :: OpExpr
  }
  | CoercibleFilterNullEmbed Bool FieldName
  deriving (Eq)
