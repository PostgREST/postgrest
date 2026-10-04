module PostgREST.Plan.MutatePlan
  ( MutatePlan (..)
  )
where

import Protolude

import PostgREST.ApiRequest.Preferences (PreferResolution)
import PostgREST.Catalog.Identifiers (FieldName, QualifiedIdentifier)
import PostgREST.Plan.Types (CoercibleField, CoercibleLogicTree)

data MutatePlan
  = Insert
      { in_ :: QualifiedIdentifier
      , insCols :: [CoercibleField]
      , onConflict :: Maybe (PreferResolution, [FieldName])
      , where_ :: [CoercibleLogicTree]
      , returning :: [FieldName]
      , insPkCols :: [FieldName]
      , applyDefs :: Bool
      }
  | Update
      { in_ :: QualifiedIdentifier
      , updCols :: [CoercibleField]
      , where_ :: [CoercibleLogicTree]
      , returning :: [FieldName]
      , applyDefs :: Bool
      }
  | Delete
      { in_ :: QualifiedIdentifier
      , where_ :: [CoercibleLogicTree]
      , returning :: [FieldName]
      }
