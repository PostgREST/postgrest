module PostgREST.Plan.MutatePlan
  ( MutatePlan(..)
  )
where

import qualified Data.ByteString.Lazy as LBS

import PostgREST.ApiRequest.Preferences  (PreferResolution)
import PostgREST.ApiRequest.Types        (LogicTree, OrderTerm)
import PostgREST.Plan.Types              (TypedField)
import PostgREST.RangeQuery              (NonnegRange)
import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier)


import Protolude

data MutatePlan
  = Insert
      { in_        :: QualifiedIdentifier
      , insCols    :: [TypedField]
      , insBody    :: Maybe LBS.ByteString
      , onConflict :: Maybe (PreferResolution, [FieldName])
      , where_     :: [LogicTree]
      , returning  :: [FieldName]
      , insPkCols  :: [FieldName]
      }
  | Update
      { in_       :: QualifiedIdentifier
      , updCols   :: [TypedField]
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
