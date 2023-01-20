module PostgREST.Plan.MutatePlan
  ( MutatePlan(..)
  )
where

import qualified Data.ByteString.Lazy as LBS

import PostgREST.ApiRequest.Preferences  (PreferResolution)
import PostgREST.ApiRequest.Types        (OrderTerm)
import PostgREST.Plan.Types              (CoercibleField,
                                          TypedLogicTree)
import PostgREST.RangeQuery              (NonnegRange)
import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier)


import Protolude

data MutatePlan
  = Insert
      { in_        :: QualifiedIdentifier
      , insCols    :: [CoercibleField]
      , insBody    :: Maybe LBS.ByteString
      , onConflict :: Maybe (PreferResolution, [FieldName])
      , where_     :: [TypedLogicTree]
      , returning  :: [FieldName]
      , insPkCols  :: [FieldName]
      }
  | Update
      { in_       :: QualifiedIdentifier
      , updCols   :: [CoercibleField]
      , updBody   :: Maybe LBS.ByteString
      , where_    :: [TypedLogicTree]
      , mutRange  :: NonnegRange
      , mutOrder  :: [OrderTerm]
      , returning :: [FieldName]
      }
  | Delete
      { in_       :: QualifiedIdentifier
      , where_    :: [TypedLogicTree]
      , mutRange  :: NonnegRange
      , mutOrder  :: [OrderTerm]
      , returning :: [FieldName]
      }
