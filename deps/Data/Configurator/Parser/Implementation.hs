{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor #-}

-- |
-- Module:      Data.Configurator.Parser.Implementation
-- Copyright:   (c) 2015-2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>

module Data.Configurator.Parser.Implementation where

#if !(MIN_VERSION_base(4,8,0))
import           Control.Applicative
#endif
import           Control.Monad (ap)
import           Data.Configurator.Config (Config)
import qualified Data.Configurator.Config as C
import           Data.Configurator.Config.Implementation (ConfigPlan(..))
import           Data.Configurator.Types (ConfigError)
import           Data.DList (DList)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Typeable (Typeable)

type RMW r w a = r -> (Maybe a, w)

type ConfigErrors = Maybe (DList ConfigError)

-- | If the value returned by a computation is 'Nothing', then no subsequent
--   actions (e.g. via '<*>' or '>>=') will be performed.

newtype ConfigParserM a
    = ConfigParserM { unConfigParserM :: RMW Config ConfigErrors a }
      deriving (Typeable, Functor)

instance Applicative ConfigParserM where
    pure a = ConfigParserM $ \_ -> (pure a, mempty)
    (<*>) = ap

instance Monad ConfigParserM where
#if !(MIN_VERSION_base(4,8,0))
    return = pure
#endif
    m >>= k  = ConfigParserM $ \r ->
                   let (ma, w ) = unConfigParserM m r
                    in case ma of
                         Nothing -> (Nothing, w)
                         Just a  -> let (mb, w') = unConfigParserM (k a) r
                                     in (mb, w <> w')

-- | After executing a subcomputation that returns a 'Nothing' value,
--   computations of type 'ConfigParserA' will continue to run in order to
--   produce more error messages.  For this reason,  'ConfigParserA' does
--   not have a proper 'Monad' instance.  (But see 'unsafeBind')

newtype ConfigParserA a
    = ConfigParserA { unConfigParserA :: RMW Config ConfigErrors a }
      deriving (Typeable, Functor)

instance Applicative ConfigParserA where
    pure a  = ConfigParserA $ \_ -> (pure a, mempty)
    f <*> a = ConfigParserA $ \r ->
                  let (mf, w ) = unConfigParserA f r
                      (ma, w') = unConfigParserA a r
                   in (mf <*> ma, w <> w')

#if __GLASGOW_HASKELL__ >= 800
{-# DEPRECATED unsafeBind "Use the ApplicativeDo language extension instead" #-}
#endif

-- |  The purpose of this function is to make it convenient to use do-notation
--    with 'ConfigParserA',  either by defining a Monad instance or locally
--    rebinding '>>='.    Be warned that this is an abuse,  and incorrect
--    usage can result in exceptions.   A safe way to use this function
--    would be to treat is as applicative-do notation.  A safer alternative
--    would be to use the @ApplicativeDo@ language extension available in
--    GHC 8.0 and not use this function at all.

unsafeBind :: ConfigParserA a -> (a -> ConfigParserA b) -> ConfigParserA b
unsafeBind m k = ConfigParserA $ \r ->
                   case unConfigParserA m r of
                     (Nothing, w) -> let (_, w')  = unConfigParserA (k err) r
                                      in (Nothing, w <> w')
                     (Just a,  w) -> let (mb, w') = unConfigParserA (k a) r
                                      in (mb, w <> w')
  where err = error "unsafeBind on ConfigParserA used incorrectly"


{--
--- There are at least three obvious "implementations" of <|> on ConfigParserM
--- TODO: check alternative laws and pick an appropriate instance for each

instance Alternative ConfigParserM where
    empty   = ConfigParserM $ \_ -> (Nothing, mempty)
    f <|> g = ConfigParserM $ \r ->
                  case unConfigParserM m0 r of
                    (Nothing, _errs0) -> unConfigParserM m1 r
                    res -> res

instance Alternative ConfigParserA where
    empty   = ConfigParserA $ \_ -> (Nothing, mempty)
    f <|> g = ConfigParserA $ \r -> let (mf, w ) = unConfigParserA f r
                                        (mg, w') = unConfigParserA g r
                                     in (mf <|> mg, w <> w')


instance Alternative ConfigParserA where
    empty   = ConfigParserA $ \_ -> (Nothing, mempty)
    f <|> g = ConfigParserA $ \r -> let (mf, w ) = unConfigParserA f r
                                        (mg, w') = unConfigParserA g r
                                     in case mf of
                                          (Just f) -> (mf, w)
                                          Nothing  -> (mg, w <> w')

--}

-- | A 'ConfigParser' computation produces a value of type @'Maybe' a@
--   from a given 'Config',  in addition to a list of diagnostic messages,
--   which may be interpreted as warnings or errors as deemed appropriate.
--   The type class abstracts over 'ConfigParserM' and 'ConfigParserA'
--   variants,  which are isomorphic but have different 'Applicative' and
--   'Monad' instances.  This is intended to be a closed typeclass, without
--   any additional instances.

class Applicative m => ConfigParser m where
    configParser_   :: RMW Config ConfigErrors a -> m a
    unConfigParser_ :: m a -> RMW Config ConfigErrors a

{--
--- Unfortunately,  this doesn't work (yet?) because of MonadReader's
--- Monad superclass.

instance ConfigParser m => MonadReader m where
     ask = configParser_ $ \c -> (Just c, mempty)


Data.Configurator.Parser.Internal

--}

instance ConfigParser ConfigParserM where
    configParser_   = ConfigParserM
    unConfigParser_ = unConfigParserM

instance ConfigParser ConfigParserA where
    configParser_   = ConfigParserA
    unConfigParser_ = unConfigParserA

-- | Conceptually, a 'ConfigTransform' is a function 'Config' @->@ 'Config'.
--   It's a restricted subset of such functions as to preserve the possibility
--   of reliable dependency tracking in later versions of configurator-ng.
newtype ConfigTransform = ConfigTransform (ConfigPlan ())

-- | 'mempty' is the identity 'ConfigTransform',  'mappend' is the composition
--   of two 'ConfigTransform's.
instance Monoid ConfigTransform where
   mempty = ConfigTransform (ConfigPlan ())
   (ConfigTransform x) `mappend` (ConfigTransform y) = (ConfigTransform (go x))
     where
       go (ConfigPlan _)      = y
       go (Union a b)         = Union (go a) (go b)
       go (Superconfig pre a) = Superconfig pre (go a)
       go (Subconfig pre a)   = Subconfig pre (go a)
       go Empty               = Empty

-- | Conceptually,  @'union' f g = \\config -> union\' (f config) (g config)@,
-- where @union\'@ is the left-biased union of two 'Config's.
union :: ConfigTransform -> ConfigTransform -> ConfigTransform
union (ConfigTransform x) (ConfigTransform y) = ConfigTransform (Union x y)

-- | @'subconfig' group@ restricts the configuration to those values that
-- are contained within @group@ (either directly,  or contained within a
-- descendant value grouping),  and removes the @group@ prefix from all
-- of the keys in the map.  It's analogous to the @cd@ (change directory)
-- command on common operating systems,  except that @subconfig@ can only
-- descend down the directory tree,  and cannot ascend into a parent
-- directory.
subconfig :: Text -> ConfigTransform -> ConfigTransform
subconfig k (ConfigTransform x) = ConfigTransform (Subconfig k x)

-- | @'superconfig' group@ adds the @group@ prefix to all keys in the map.
-- It is vaguely analogous to the @mount@ command on unix operating systems.
superconfig :: Text -> ConfigTransform -> ConfigTransform
superconfig k (ConfigTransform x) = ConfigTransform (Superconfig k x)

interpConfigTransform :: ConfigTransform -> Config -> Config
interpConfigTransform (ConfigTransform x) config = go x
  where
    go Empty             = C.empty
    go (ConfigPlan _)    = config
    go (Superconfig k x) = C.superconfig k (go x)
    go (Subconfig   k x) = C.subconfig k (go x)
    go (Union       x y) = C.union (go x) (go y)
