{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections   #-}

-- |
-- Module:      Data.Configurator.Parser
-- Copyright:   (c) 2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- A set of combinators for high-level configuration parsing.

module Data.Configurator.Parser
    (
    -- * High level parsing computations
      ConfigParser
    , runParser
    , ConfigParserA
    , runParserA
    , parserA
    , unsafeBind
    , ConfigParserM
    , runParserM
    , parserM
    , recover
    -- * Looking up values by name
    , key
    , keyWith
    -- * Discovering names
    , subgroups
    , subassocs
    , subassocs'
    -- * Modifying the configuration context
    , Config
    , ConfigTransform
    , localConfig
    , union
    , subconfig
    , superconfig
    -- * Error / warning messages
    , ConfigError (..)
    , ConfigErrorLocation (..)
    , ConversionError (..)
    , ConversionErrorWhy (..)
    ) where

import           Prelude hiding (null)

import           Data.DList (DList)
import qualified Data.DList as DL

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid(Monoid(..))
#endif
import           Data.Monoid((<>))
import           Data.Configurator.Config
                   ( Config )
import           Data.Configurator.Types.Internal hiding (Group)
import           Data.Configurator.FromValue
                   ( FromMaybeValue(fromMaybeValue)
                   , MaybeParser
                   , runMaybeParser
                   )
import qualified Data.Configurator.Config as C
import qualified Data.Configurator.Config.Internal as CI
import           Data.Configurator.Parser.Implementation

runParser :: ConfigParser m => m a -> Config -> (Maybe a, [ConfigError])
runParser m conf = let (ma, errs) = unConfigParser_ m conf
                    in (ma, toErrors errs)

{- |
Returns all the value bindings from the current configuration context that is
contained within the given subgroup, in lexicographic order. For example,
given the following context:

@
x = 1
foo {
  x = 2
  bar {
    y = on
  }
}
foo = \"Hello\"
@

Then the following arguments to 'subassocs' would return the following lists:

@
subassocs \"\"         ==>  [(\"foo\",String \"Hello\"),(\"x\",Number 1)]
subassocs \"foo\"      ==>  [(\"foo.x\",Number 2)]
subassocs \"foo.bar\"  ==>  [(\"foo.bar.x\",Bool True)]
@

All other arguments to @subassocs@ would return @[]@ in the given context.
-}

subassocs :: ConfigParser m => Name -> m [(Name, Value)]
subassocs t = configParser_ (\c -> (Just (C.subassocs t c), mempty))

{- |
Returns all the value bindings from the current configuration context that is
contained within the given subgroup and all of it's subgroups in lexicographic
order. For example, given the following context:

@
x = 1
foo {
  x = 2
  bar {
    y = on
  }
}
foo = \"Hello\"
@

Then the following arguments to 'subassocs\'' would return the following lists:

@
subassocs\' \"\"         ==>  [ (\"foo\"       , String \"Hello\")
                           , (\"foo.bar.y\" , Bool True     )
                           , (\"foo.x\"     , Number 2      )
                           , (\"x\"         , Number 1      )
                           ]
subassocs\' \"foo\"      ==>  [ (\"foo.bar.y\" , Bool True     )
                           , (\"foo.x\"     , Number 2      )
                           ]
subassocs\' \"foo.bar\"  ==>  [ (\"foo.bar.y\" , Bool True     )
                           ]
@

All other arguments to @subassocs\'@ would return @[]@ in the given context.
-}

subassocs' :: ConfigParser m => Name -> m [(Name, Value)]
subassocs' t = configParser_ (\c -> (Just (C.subassocs' t c), mempty))

{- |
Returns all the non-empty value groupings that is directly under the argument
grouping in the current configuration context.  For example, given the
following context:

@
foo { }
bar {
  a {
    x = 1
  }
  b {
    c {
      y = 2
    }
  }
}
default
  a {
    x = 3
  }
}
@

Then the following arguments to 'subgroups' would return the following lists:

@
subgroups \"\"         ==>  [ \"bar\", \"default\" ]
subgroups \"bar\"      ==>  [ \"bar.a\", \"bar.b\" ]
subgroups \"bar.b\"    ==>  [ \"bar.b.c\" ]
subgroups \"default\"  ==>  [ \"default.a\" ]
@

All other arguments to @subgroups@ would return @[]@ in the given context.
-}

subgroups :: ConfigParser m => Name -> m [Name]
subgroups t = configParser_ (\c -> (Just (C.subgroups t c), mempty))

-- |  Modifies the 'Config' that a subparser is operating on.
--    This is perfectly analogous to 'Control.Monad.Reader.local'.

localConfig :: ConfigParser m => ConfigTransform -> m a -> m a
localConfig f m = configParser_ (\r -> unConfigParser_ m (interpConfigTransform f r))

-- |  Exactly the same as 'runParser',  except less polymorphic

runParserA :: ConfigParserA a -> Config -> (Maybe a, [ConfigError])
runParserA = runParser

-- |  Exactly the same as 'runParser',  except less polymorphic

runParserM :: ConfigParserM a -> Config -> (Maybe a, [ConfigError])
runParserM = runParser

-- |  Lift a 'ConfigParserM' action into a generic 'ConfigParser'
--    action.  Note that this does not change the semantics of the
--    argument,  it just allows a 'ConfigParserM' computation to be
--    embedded in another 'ConfigParser' computation of either variant.

parserM :: ConfigParser m => ConfigParserM a -> m a
parserM (ConfigParserM m) = configParser_ m

-- |  Lift a 'ConfigParserA' action into a generic 'ConfigParser'
--    action.  Note that this does not change the semantics of the
--    argument,  it just allows a 'ConfigParserA' computation to be
--    embedded in another 'ConfigParser' computation of either variant.

parserA :: ConfigParser m => ConfigParserA a -> m a
parserA (ConfigParserA m) = configParser_ m

-- |  Given the expression @'recover' action@, the @action@ will be
--    run,  and if it returns no value,  @recover action@ will return
--    'Nothing'.   If @action@ returns the value @a@, then
--    @recover action@ will return the value @'Just' a@.  Any errors
--    or warnings are passed through as-is.

recover :: ConfigParser m => m a -> m (Maybe a)
recover m = configParser_ $ \r -> let (ma, errs) = unConfigParser_ m r
                                   in (Just ma, errs)

-- | Look up a given value in the current configuration context,  and convert
-- the value using the 'fromMaybeValue' method.

key :: (ConfigParser m, FromMaybeValue a) => Name -> m a
key name = keyWith name fromMaybeValue

-- | Look up a given value in the current configuration context,  and convert
-- the value using the 'MaybeParser' argument.

keyWith :: (ConfigParser m) => Name -> MaybeParser a -> m a
keyWith name parser =
    configParser_ $ \(CI.Config c) ->
        case CI.lookupWithName name c of
          Nothing ->
              convert (KeyMissing (DL.toList (getLookupPlan name c))) Nothing
          Just (name', v) ->
              convert (Key "" name') (Just v)
  where
    convert loc mv =
        case runMaybeParser parser mv of
          (Nothing, errs) ->
              (Nothing, singleError (ConfigError loc (Just errs)))
          (Just a, []) ->
              (Just a, mempty)
          (Just a, errs@(_:_)) ->
              (Just a,  singleError (ConfigError loc (Just errs)))

getLookupPlan :: Name -> CI.ConfigPlan a -> DList Name
getLookupPlan = CI.foldPlan DL.empty (<>) (\k _ -> DL.singleton k)
