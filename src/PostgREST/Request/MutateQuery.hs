module PostgREST.Request.MutateQuery
  ( MutateQuery(..)
  , MutateRequest
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set             as S

import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier)
import PostgREST.RangeQuery              (NonnegRange)
import PostgREST.Request.Preferences     (PreferResolution)
import PostgREST.Request.Types           (LogicTree, OrderTerm)

import Protolude

type MutateRequest = MutateQuery

data MutateQuery
  = Insert
      { in_        :: QualifiedIdentifier
      , insCols    :: S.Set FieldName
      , insBody    :: Maybe LBS.ByteString
      , onConflict :: Maybe (PreferResolution, [FieldName])
      , where_     :: [LogicTree]
      , returning  :: [FieldName]
      }
  | Update
      { in_       :: QualifiedIdentifier
      , updCols   :: S.Set FieldName
      , updBody   :: Maybe LBS.ByteString
      , where_    :: [LogicTree]
      , mutRange  :: NonnegRange
      , mutOrder  :: [OrderTerm]
      , returning :: [FieldName]
      }
  | Delete
      { in_       :: QualifiedIdentifier
      , where_    :: [LogicTree]
      , mutRange  :: NonnegRange
      , mutOrder  :: [OrderTerm]
      , returning :: [FieldName]
      }
