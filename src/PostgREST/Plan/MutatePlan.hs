module PostgREST.Plan.MutatePlan
  ( MutatePlan(..)
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set             as S

import PostgREST.ApiRequest.Preferences  (PreferResolution)
import PostgREST.ApiRequest.Types        (LogicTree, OrderTerm)
import PostgREST.DbStructure.Identifiers (FieldName,
                                          QualifiedIdentifier)
import PostgREST.RangeQuery              (NonnegRange)

import Protolude

data MutatePlan
  = Insert
      { in_        :: QualifiedIdentifier
      , insCols    :: S.Set FieldName
      , insBody    :: Maybe LBS.ByteString
      , onConflict :: Maybe (PreferResolution, [FieldName])
      , where_     :: [LogicTree]
      , returning  :: [FieldName]
      , insPkCols  :: [FieldName]
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
