-- |
-- Module:      Data.Configurator.Config
-- Copyright:   (c) 2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
--
-- This module provides the abstract data structure that backs @ConfigCache@
-- and that @ConfigParser@s operate on.
--
-- It shouldn't be necessary to use this module much, if at all, in client
-- code.  It might be considered semi-internal.  Please file a issue if you
-- find a need to use it.

module Data.Configurator.Config
     ( Config
     , empty
     , null
     , lookup
     , lookupWithName
     , subgroups
     , subassocs
     , subassocs'
     , union
     , subconfig
     , superconfig
     ) where

import Prelude hiding (lookup,null)
import Data.Configurator.Types(Name,Value)
import Data.Configurator.Config.Implementation(Config(..),ConfigPlan(Empty))
import qualified Data.Configurator.Config.Implementation as C

lookup :: Name -> Config -> Maybe Value
lookup k (Config c) = C.lookup k c

lookupWithName :: Name -> Config -> Maybe (Name, Value)
lookupWithName k (Config c) = C.lookupWithName k c

subgroups :: Name -> Config -> [Name]
subgroups k (Config c) = C.subgroups k c

subassocs :: Name -> Config -> [(Name,Value)]
subassocs k (Config c) = C.subassocs k c

subassocs' :: Name -> Config -> [(Name,Value)]
subassocs' k (Config c) = C.subassocs' k c

empty :: Config
empty =  Config Empty

null :: Config -> Bool
null (Config c) = C.null c

union :: Config -> Config -> Config
union (Config a) (Config b) = Config (C.union a b)

subconfig :: Name -> Config -> Config
subconfig key (Config c) = Config (C.subconfig key c)

superconfig :: Name -> Config -> Config
superconfig key (Config c) = Config (C.superconfig key c)
