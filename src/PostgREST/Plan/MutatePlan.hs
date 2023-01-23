module PostgREST.Plan.MutatePlan
  ( MutatePlan(..)
  )
where

import qualified Data.ByteString.Lazy as LBS

import PostgREST.ApiRequest.Preferences  (PreferResolution)
import PostgREST.ApiRequest.Types        (OrderTerm)
import PostgREST.Plan.Types              (CoercibleField,
                                          CoercibleLogicTree)
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
      , where_     :: [CoercibleLogicTree]
      , returning  :: [FieldName]
      , insPkCols  :: [FieldName]
      }
  | Update
      { in_       :: QualifiedIdentifier
      , updCols   :: [CoercibleField]
      , updBody   :: Maybe LBS.ByteString
      , where_    :: [CoercibleLogicTree]
      , mutRange  :: NonnegRange
      , mutOrder  :: [OrderTerm]
      , returning :: [FieldName]
      }
  | Delete
      { in_       :: QualifiedIdentifier
      , where_    :: [CoercibleLogicTree]
      , mutRange  :: NonnegRange
      , mutOrder  :: [OrderTerm]
      , returning :: [FieldName]
      }
