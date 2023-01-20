{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostgREST.SchemaCache.Representations
  ( DataRepresentation(..)
  , RepresentationsMap
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM


import Protolude

-- | Data representations allow user customisation of how to present and receive data through APIs, per field.
-- This structure is used for the library of available transforms. It answers questions like:
-- - What function, if any, should be used to present a certain field that's been selected for API output?
-- - How do we parse incoming data for a certain field type when inserting or updating?
-- - And similarly, how do we parse textual data in a query string to be used as a filter?
--
-- Support for outputting special formats like CSV and binary data would fit into the same system.
data DataRepresentation = DataRepresentation
  { drSourceType :: Text
  , drTargetType :: Text
  , drFunction   :: Text
  } deriving (Eq, Show, Generic, JSON.ToJSON, JSON.FromJSON)

-- The representation map maps from (source type, target type) to a DR.
type RepresentationsMap = HM.HashMap (Text, Text) DataRepresentation
