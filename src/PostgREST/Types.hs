{-|
Module      : PostgREST.Types
Description : PostgREST common types and functions used by the rest of the modules
-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.Types where


import qualified Data.Aeson               as JSON
import qualified Data.ByteString.Lazy     as BL
import qualified Data.CaseInsensitive     as CI
import qualified Data.HashMap.Strict      as M
import qualified Data.Set                 as S

import Control.Lens.Getter (view)
import Control.Lens.Tuple  (_1)
import Data.Tree (Tree(..))
import Network.HTTP.Types.Header (Header)

import qualified GHC.Show (show)

import PostgREST.RangeQuery (NonnegRange)
import PostgREST.DbStructureTypes

import Protolude            hiding (toS)
import Protolude.Conv       (toS)


-- | A SQL query that can be executed independently
type SqlQuery = ByteString

-- | A part of a SQL query that cannot be executed independently
type SqlFragment = ByteString

data PreferResolution = MergeDuplicates | IgnoreDuplicates
instance Show PreferResolution where
  show MergeDuplicates  = "resolution=merge-duplicates"
  show IgnoreDuplicates = "resolution=ignore-duplicates"

-- | How to return the mutated data. From https://tools.ietf.org/html/rfc7240#section-4.2
data PreferRepresentation = Full        -- ^ Return the body plus the Location header(in case of POST).
                          | HeadersOnly -- ^ Return the Location header(in case of POST). This needs a SELECT privilege on the pk.
                          | None        -- ^ Return nothing from the mutated data.
                          deriving Eq
instance Show PreferRepresentation where
  show Full        = "return=representation"
  show None        = "return=minimal"
  show HeadersOnly = mempty

data PreferParameters
  = SingleObject    -- ^ Pass all parameters as a single json object to a stored procedure
  | MultipleObjects -- ^ Pass an array of json objects as params to a stored procedure
  deriving Eq

instance Show PreferParameters where
  show SingleObject    = "params=single-object"
  show MultipleObjects = "params=multiple-objects"

data PreferCount
  = ExactCount     -- ^ exact count(slower)
  | PlannedCount   -- ^ PostgreSQL query planner rows count guess. Done by using EXPLAIN {query}.
  | EstimatedCount -- ^ use the query planner rows if the count is superior to max-rows, otherwise get the exact count.
  deriving Eq

instance Show PreferCount where
  show ExactCount     = "count=exact"
  show PlannedCount   = "count=planned"
  show EstimatedCount = "count=estimated"

data PreferTransaction
  = Commit   -- Commit transaction - the default.
  | Rollback -- Rollback transaction after sending the response - does not persist changes, e.g. for running tests.
  deriving Eq

instance Show PreferTransaction where
  show Commit   = "tx=commit"
  show Rollback = "tx=rollback"

-- | The source table column a view column refers to
type SourceColumn = (Column, ViewColumn)
type ViewColumn = Column

data OrderDirection = OrderAsc | OrderDesc deriving (Eq)
instance Show OrderDirection where
  show OrderAsc  = "ASC"
  show OrderDesc = "DESC"

data OrderNulls = OrderNullsFirst | OrderNullsLast deriving (Eq)
instance Show OrderNulls where
  show OrderNullsFirst = "NULLS FIRST"
  show OrderNullsLast  = "NULLS LAST"

data OrderTerm = OrderTerm {
  otTerm      :: Field
, otDirection :: Maybe OrderDirection
, otNullOrder :: Maybe OrderNulls
} deriving (Eq)

data PayloadJSON =
  -- | Cached attributes of a JSON payload
  ProcessedJSON {
    -- | This is the raw ByteString that comes from the request body.
    -- We cache this instead of an Aeson Value because it was detected that for large payloads the encoding
    -- had high memory usage, see https://github.com/PostgREST/postgrest/pull/1005 for more details
    pjRaw  :: BL.ByteString
    -- | Keys of the object or if it's an array these keys are guaranteed to be the same across all its objects
  , pjKeys :: S.Set Text
  }|
  RawJSON {
    pjRaw  :: BL.ByteString
  }

data PJType = PJArray { pjaLength :: Int } | PJObject

data Proxy = Proxy {
  proxyScheme :: Text
, proxyHost   :: Text
, proxyPort   :: Integer
, proxyPath   :: Text
}

type Operator = Text
operators :: M.HashMap Operator SqlFragment
operators = M.union (M.fromList [
  ("eq", "="),
  ("gte", ">="),
  ("gt", ">"),
  ("lte", "<="),
  ("lt", "<"),
  ("neq", "<>"),
  ("like", "LIKE"),
  ("ilike", "ILIKE"),
  ("in", "IN"),
  ("is", "IS"),
  ("cs", "@>"),
  ("cd", "<@"),
  ("ov", "&&"),
  ("sl", "<<"),
  ("sr", ">>"),
  ("nxr", "&<"),
  ("nxl", "&>"),
  ("adj", "-|-")]) ftsOperators

ftsOperators :: M.HashMap Operator SqlFragment
ftsOperators = M.fromList [
  ("fts", "@@ to_tsquery"),
  ("plfts", "@@ plainto_tsquery"),
  ("phfts", "@@ phraseto_tsquery"),
  ("wfts", "@@ websearch_to_tsquery")
  ]

data OpExpr = OpExpr Bool Operation deriving (Eq)
data Operation = Op Operator SingleVal |
                 In ListVal |
                 Fts Operator (Maybe Language) SingleVal deriving (Eq)
type Language = Text

-- | Represents a single value in a filter, e.g. id=eq.singleval
type SingleVal = Text
-- | Represents a list value in a filter, e.g. id=in.(val1,val2,val3)
type ListVal = [Text]

data LogicOperator = And | Or deriving Eq
instance Show LogicOperator where
  show And = "AND"
  show Or  = "OR"
{-|
  Boolean logic expression tree e.g. "and(name.eq.N,or(id.eq.1,id.eq.2))" is:

            And
           /   \
  name.eq.N     Or
               /  \
         id.eq.1   id.eq.2
-}
data LogicTree = Expr Bool LogicOperator [LogicTree] | Stmnt Filter deriving (Eq)

{-|
  Json path operations as specified in https://www.postgresql.org/docs/current/static/functions-json.html
-}
type JsonPath = [JsonOperation]
-- | Represents the single arrow `->` or double arrow `->>` operators
data JsonOperation = JArrow{jOp :: JsonOperand} | J2Arrow{jOp :: JsonOperand} deriving (Eq)
-- | Represents the key(`->'key'`) or index(`->'1`::int`), the index is Text because we reuse our escaping functons and let pg do the casting with '1'::int
data JsonOperand = JKey{jVal :: Text} | JIdx{jVal :: Text} deriving (Eq)

type Field = (FieldName, JsonPath)
type Alias = Text
type Cast = Text
type NodeName = Text


{-|
  Custom guc header, it's obtained by parsing the json in a:
  `SET LOCAL "response.headers" = '[{"Set-Cookie": ".."}]'
-}
newtype GucHeader = GucHeader (CI.CI ByteString, ByteString)

instance JSON.FromJSON GucHeader where
  parseJSON (JSON.Object o) = case headMay (M.toList o) of
    Just (k, JSON.String s) | M.size o == 1 -> pure $ GucHeader (CI.mk $ toS k, toS s)
                            | otherwise     -> mzero
    _ -> mzero
  parseJSON _          = mzero

unwrapGucHeader :: GucHeader -> Header
unwrapGucHeader (GucHeader (k, v)) = (k, v)

-- | Add headers not already included to allow the user to override them instead of duplicating them
addHeadersIfNotIncluded :: [Header] -> [Header] -> [Header]
addHeadersIfNotIncluded newHeaders initialHeaders =
  filter (\(nk, _) -> isNothing $ find (\(ik, _) -> ik == nk) initialHeaders) newHeaders ++
  initialHeaders

{-|
  This type will hold information about which particular 'Relation' between two tables to choose when there are multiple ones.
  Specifically, it will contain the name of the foreign key or the join table in many to many relations.
-}
type SelectItem = (Field, Maybe Cast, Maybe Alias, Maybe EmbedHint)
-- | Disambiguates an embedding operation when there's multiple relationships between two tables.
-- | Can be the name of a foreign key constraint, column name or the junction in an m2m relationship.
type EmbedHint = Text
-- | Path of the embedded levels, e.g "clients.projects.name=eq.." gives Path ["clients", "projects"]
type EmbedPath = [Text]
data Filter = Filter { field::Field, opExpr::OpExpr } deriving (Eq)
data JoinCondition = JoinCondition (QualifiedIdentifier, FieldName)
                                   (QualifiedIdentifier, FieldName) deriving (Eq)

data ReadQuery = Select {
    select         :: [SelectItem]
  , from           :: QualifiedIdentifier
-- | A table alias is used in case of self joins
  , fromAlias      :: Maybe Alias
-- | Only used for Many to Many joins. Parent and Child joins use explicit joins.
  , implicitJoins  :: [QualifiedIdentifier]
  , where_         :: [LogicTree]
  , joinConditions :: [JoinCondition]
  , order          :: [OrderTerm]
  , range_         :: NonnegRange
} deriving (Eq)

data MutateQuery =
  Insert {
    in_        :: QualifiedIdentifier
  , insCols    :: S.Set FieldName
  , insBody    :: Maybe BL.ByteString
  , onConflict :: Maybe (PreferResolution, [FieldName])
  , where_     :: [LogicTree]
  , returning  :: [FieldName]
  }|
  Update {
    in_       :: QualifiedIdentifier
  , updCols   :: S.Set FieldName
  , updBody   :: Maybe BL.ByteString
  , where_    :: [LogicTree]
  , returning :: [FieldName]
  }|
  Delete {
    in_       :: QualifiedIdentifier
  , where_    :: [LogicTree]
  , returning :: [FieldName]
  }

type ReadRequest = Tree ReadNode
type MutateRequest = MutateQuery

type ReadNode = (ReadQuery, (NodeName, Maybe Relation, Maybe Alias, Maybe EmbedHint, Depth))
type Depth = Integer

-- First level FieldNames(e.g get a,b from /table?select=a,b,other(c,d))
fstFieldNames :: ReadRequest -> [FieldName]
fstFieldNames (Node (sel, _) _) =
  fst . view _1 <$> select sel

-- | Tells the minimum PostgreSQL version required by this version of PostgREST
minimumPgVersion :: PgVersion
minimumPgVersion = pgVersion95

pgVersion95 :: PgVersion
pgVersion95 = PgVersion 90500 "9.5"

pgVersion96 :: PgVersion
pgVersion96 = PgVersion 90600 "9.6"

pgVersion100 :: PgVersion
pgVersion100 = PgVersion 100000 "10"

pgVersion109 :: PgVersion
pgVersion109 = PgVersion 100009 "10.9"

pgVersion110 :: PgVersion
pgVersion110 = PgVersion 110000 "11.0"

pgVersion112 :: PgVersion
pgVersion112 = PgVersion 110002 "11.2"

pgVersion114 :: PgVersion
pgVersion114 = PgVersion 110004 "11.4"

pgVersion121 :: PgVersion
pgVersion121 = PgVersion 120001 "12.1"

pgVersion130 :: PgVersion
pgVersion130 = PgVersion 130000 "13.0"

sourceCTEName :: SqlFragment
sourceCTEName = "pgrst_source"

-- | full jspath, e.g. .property[0].attr.detail
type JSPath = [JSPathExp]
-- | jspath expression, e.g. .property, .property[0] or ."property-dash"
data JSPathExp = JSPKey Text | JSPIdx Int

instance Show JSPathExp where
  -- TODO: this needs to be quoted properly for special chars
  show (JSPKey k) = "." <> show k
  show (JSPIdx i) = "[" <> show i <> "]"

-- | Schema cache status
data SCacheStatus
  = SCLoaded
  | SCOnRetry
  | SCFatalFail

data LogLevel = LogCrit | LogError | LogWarn | LogInfo

instance Show LogLevel where
  show LogCrit  = "crit"
  show LogError = "error"
  show LogWarn  = "warn"
  show LogInfo  = "info"
