{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Functor (
  Functor(fmap),
  ($>),
  (<$),
  (<$>),
  (<<$>>),
  (<&>),
  void,
  foreach,
) where

import Data.Function ((.), flip)
#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Functor (
    Functor(fmap)
  , (<$)
  , ($>)
  , (<$>)
  , void
  )
#else
import Data.Functor (
    Functor(fmap)
  , (<$)
  , (<$>)
  )


infixl 4 $>

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

void :: Functor f => f a -> f ()
void x = () <$ x
#endif

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip fmap

#if !MIN_VERSION_base(4,11,0)
-- | Infix version of foreach.
--
-- '<&>' is to '<$>' what '&' is to '$'.

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = foreach
#endif
