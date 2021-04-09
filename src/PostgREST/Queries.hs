{-# LANGUAGE DuplicateRecordFields #-}
module PostgREST.Queries
  ( Alias
  , Depth
  , EmbedHint
  , EmbedPath
  , Field
  , Filter(..)
  , JoinCondition(..)
  , JsonOperand(..)
  , JsonOperation(..)
  , JsonPath
  , ListVal
  , LogicOperator(..)
  , LogicTree(..)
  , MutateQuery(..)
  , MutateRequest
  , NodeName
  , OpExpr(..)
  , Operation (..)
  , OrderDirection(..)
  , OrderNulls(..)
  , OrderTerm(..)
  , ReadNode
  , ReadQuery(..)
  , ReadRequest
  , SelectItem
  , SingleVal
  , fstFieldNames
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Set             as S

import Data.Tree (Tree (..))

import qualified GHC.Show (show)

import PostgREST.ApiRequest.Preferences  (PreferResolution)
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier)
import PostgREST.DbStructure.Relation    (Relation)
import PostgREST.RangeQuery              (NonnegRange)

import Protolude


type ReadRequest = Tree ReadNode
type MutateRequest = MutateQuery

type ReadNode =
  (ReadQuery, (NodeName, Maybe Relation, Maybe Alias, Maybe EmbedHint, Depth))

type NodeName = Text
type Depth = Integer

data ReadQuery = Select
  { select         :: [SelectItem]
  , from           :: QualifiedIdentifier
  -- ^ A table alias is used in case of self joins
  , fromAlias      :: Maybe Alias
  -- ^ Only used for Many to Many joins. Parent and Child joins use explicit joins.
  , implicitJoins  :: [QualifiedIdentifier]
  , where_         :: [LogicTree]
  , joinConditions :: [JoinCondition]
  , order          :: [OrderTerm]
  , range_         :: NonnegRange
  }
  deriving (Eq)

data JoinCondition =
  JoinCondition
    (QualifiedIdentifier, FieldName)
    (QualifiedIdentifier, FieldName)
  deriving (Eq)

data OrderTerm = OrderTerm
  { otTerm      :: Field
  , otDirection :: Maybe OrderDirection
  , otNullOrder :: Maybe OrderNulls
  }
  deriving (Eq)

data OrderDirection
  = OrderAsc
  | OrderDesc
  deriving (Eq)

instance Show OrderDirection where
  show OrderAsc  = "ASC"
  show OrderDesc = "DESC"

data OrderNulls
  = OrderNullsFirst
  | OrderNullsLast
  deriving (Eq)

instance Show OrderNulls where
  show OrderNullsFirst = "NULLS FIRST"
  show OrderNullsLast  = "NULLS LAST"

data MutateQuery
  = Insert
      { in_        :: QualifiedIdentifier
      , insCols    :: S.Set FieldName
      , insBody    :: Maybe BL.ByteString
      , onConflict :: Maybe (PreferResolution, [FieldName])
      , where_     :: [LogicTree]
      , returning  :: [FieldName]
      }
  | Update
      { in_       :: QualifiedIdentifier
      , updCols   :: S.Set FieldName
      , updBody   :: Maybe BL.ByteString
      , where_    :: [LogicTree]
      , returning :: [FieldName]
      }
  | Delete
      { in_       :: QualifiedIdentifier
      , where_    :: [LogicTree]
      , returning :: [FieldName]
      }

-- | This type will hold information about which particular 'Relation' between
-- two tables to choose when there are multiple ones.
-- Specifically, it will contain the name of the foreign key or the join table
-- in many to many relations.
type SelectItem = (Field, Maybe Cast, Maybe Alias, Maybe EmbedHint)

type Field = (FieldName, JsonPath)
type Cast = Text
type Alias = Text

-- | Disambiguates an embedding operation when there's multiple relationships
-- between two tables. Can be the name of a foreign key constraint, column
-- name or the junction in an m2m relationship.
type EmbedHint = Text

-- | Path of the embedded levels, e.g "clients.projects.name=eq.." gives Path
-- ["clients", "projects"]
type EmbedPath = [Text]

-- | Json path operations as specified in
-- https://www.postgresql.org/docs/current/static/functions-json.html
type JsonPath = [JsonOperation]

-- | Represents the single arrow `->` or double arrow `->>` operators
data JsonOperation
  = JArrow { jOp :: JsonOperand }
  | J2Arrow { jOp :: JsonOperand }
  deriving (Eq)

-- | Represents the key(`->'key'`) or index(`->'1`::int`), the index is Text
-- because we reuse our escaping functons and let pg do the casting with
-- '1'::int
data JsonOperand
  = JKey { jVal :: Text }
  | JIdx { jVal :: Text }
  deriving (Eq)

-- First level FieldNames(e.g get a,b from /table?select=a,b,other(c,d))
fstFieldNames :: ReadRequest -> [FieldName]
fstFieldNames (Node (sel, _) _) =
  fst . (\(f, _, _, _) -> f) <$> select sel


-- | Boolean logic expression tree e.g. "and(name.eq.N,or(id.eq.1,id.eq.2))" is:
--
--            And
--           /   \
--  name.eq.N     Or
--               /  \
--         id.eq.1   id.eq.2
data LogicTree
  = Expr Bool LogicOperator [LogicTree]
  | Stmnt Filter
  deriving (Eq)

data LogicOperator
  = And
  | Or
  deriving Eq

instance Show LogicOperator where
  show And = "AND"
  show Or  = "OR"

data Filter = Filter
  { field  :: Field
  , opExpr :: OpExpr
  }
  deriving (Eq)

data OpExpr =
  OpExpr Bool Operation
  deriving (Eq)

data Operation
  = Op Operator SingleVal
  | In ListVal
  | Fts Operator (Maybe Language) SingleVal
  deriving (Eq)

type Operator = Text
type Language = Text

-- | Represents a single value in a filter, e.g. id=eq.singleval
type SingleVal = Text

-- | Represents a list value in a filter, e.g. id=in.(val1,val2,val3)
type ListVal = [Text]
