{-# LANGUAGE DuplicateRecordFields #-}
module PostgREST.Request.Types
  ( Alias
  , Cast
  , Depth
  , EmbedParam(..)
  , ApiRequestError(..)
  , EmbedPath
  , Field
  , Filter(..)
  , Hint
  , CallQuery(..)
  , CallParams(..)
  , CallRequest
  , JoinCondition(..)
  , JoinType(..)
  , JsonOperand(..)
  , JsonOperation(..)
  , JsonPath
  , ListVal
  , LogicOperator(..)
  , LogicTree(..)
  , NodeName
  , OpExpr(..)
  , Operation (..)
  , OrderDirection(..)
  , OrderNulls(..)
  , OrderTerm(..)
  , QPError(..)
  , SingleVal
  , TrileanVal(..)
  , SimpleOperator(..)
  , FtsOperator(..)
  , BodyOperator(..)
  ) where

import qualified Data.ByteString.Lazy as LBS

import PostgREST.DbStructure.Identifiers  (FieldName,
                                           QualifiedIdentifier)
import PostgREST.DbStructure.Proc         (ProcDescription (..),
                                           ProcParam (..))
import PostgREST.DbStructure.Relationship (Relationship)
import PostgREST.MediaType                (MediaType (..))

import Protolude



data ApiRequestError
  = AmbiguousRelBetween Text Text [Relationship]
  | AmbiguousRpc [ProcDescription]
  | BodyFilterNotAllowed Text
  | MediaTypeError [ByteString]
  | InvalidBody ByteString
  | InvalidFilters
  | InvalidRange
  | InvalidRpcMethod ByteString
  | LimitNoOrderError
  | NotFound
  | NoRelBetween Text Text Text
  | NoRpc Text Text [Text] Bool MediaType Bool
  | NotEmbedded Text
  | ParseRequestError Text Text
  | PutRangeNotAllowedError
  | QueryParamError QPError
  | UnacceptableSchema [Text]
  | UnsupportedMethod ByteString

data QPError = QPError Text Text

type CallRequest = CallQuery

type NodeName = Text
type Depth = Integer

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

data OrderNulls
  = OrderNullsFirst
  | OrderNullsLast
  deriving (Eq)

data CallQuery = FunctionCall
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

type Field = (FieldName, JsonPath)
type Cast = Text
type Alias = Text
type Hint = Text

data EmbedParam
  -- | Disambiguates an embedding operation when there's multiple relationships
  -- between two tables. Can be the name of a foreign key constraint, column
  -- name or the junction in an m2m relationship.
  = EPHint Hint
  | EPJoinType JoinType

data JoinType
  = JTInner
  | JTLeft
  deriving Eq

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

data Filter = Filter
  { field  :: Field
  , opExpr :: OpExpr
  }
  deriving (Eq)

data OpExpr =
  OpExpr Bool Operation
  deriving (Eq)

data Operation
  = Op SimpleOperator SingleVal
  | In ListVal
  | Is TrileanVal
  | Fts FtsOperator (Maybe Language) SingleVal
  | BodOp BodyOperator SingleVal
  deriving (Eq)

type Language = Text

-- | Represents a single value in a filter, e.g. id=eq.singleval
type SingleVal = Text

-- | Represents a list value in a filter, e.g. id=in.(val1,val2,val3)
type ListVal = [Text]

-- | Three-valued logic values
data TrileanVal
  = TriTrue
  | TriFalse
  | TriNull
  | TriUnknown
  deriving Eq

data SimpleOperator
  = OpEqual
  | OpGreaterThanEqual
  | OpGreaterThan
  | OpLessThanEqual
  | OpLessThan
  | OpNotEqual
  | OpLike
  | OpILike
  | OpContains
  | OpContained
  | OpOverlap
  | OpStrictlyLeft
  | OpStrictlyRight
  | OpNotExtendsRight
  | OpNotExtendsLeft
  | OpAdjacent
  | OpMatch
  | OpIMatch
  deriving Eq

-- | Operators for full text search operators
data FtsOperator
  = FilterFts
  | FilterFtsPlain
  | FilterFtsPhrase
  | FilterFtsWebsearch
  deriving Eq

-- | Operators for filtering using the request body
data BodyOperator
  = BodyOpEqual
  deriving Eq
