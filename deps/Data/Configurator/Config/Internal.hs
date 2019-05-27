-- |
-- Module:      Data.Configurator.Config.Internal
-- Copyright:   (c) 2015-2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>

module Data.Configurator.Config.Internal
     ( Config(..)
     , ConfigMap
     , ConfigPlan(..)

     , lookup
     , lookupWithName
     , subgroups
     , subassocs

     , null
     , subconfig
     , superconfig
     , union

     , subassocs_
     , foldPlan
     , submap
     , subgroupsMap
     , addPrefix
     , stripPrefix
     ) where

import Prelude hiding (lookup, null)
import Data.Configurator.Config.Implementation
