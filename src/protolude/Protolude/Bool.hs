{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Bool (
  whenM
, unlessM
, ifM
, guardM
, bool
, (&&^)
, (||^)
, (<&&>)
, (<||>)
) where

import Data.Bool (Bool(True, False), (&&), (||))
import Data.Function (flip)
import Control.Applicative(Applicative, liftA2)
import Control.Monad (Monad, MonadPlus, return, when, unless, guard, (>>=), (=<<))

bool :: a -> a -> Bool -> a
bool f t p = if p then t else f

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

guardM :: MonadPlus m => m Bool -> m ()
guardM f = guard =<< f

-- | The '||' operator lifted to a monad. If the first
--   argument evaluates to 'True' the second argument will not
--   be evaluated.
infixr 2 ||^ -- same as (||)
(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (return True) b

infixr 2 <||>
-- | '||' lifted to an Applicative.
-- Unlike '||^' the operator is __not__ short-circuiting.
(<||>) :: Applicative a => a Bool -> a Bool -> a Bool
(<||>) = liftA2 (||)
{-# INLINE (<||>) #-}

-- | The '&&' operator lifted to a monad. If the first
--   argument evaluates to 'False' the second argument will not
--   be evaluated.
infixr 3 &&^ -- same as (&&)
(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) a b = ifM a b (return False)

infixr 3 <&&>
-- | '&&' lifted to an Applicative.
-- Unlike '&&^' the operator is __not__ short-circuiting.
(<&&>) :: Applicative a => a Bool -> a Bool -> a Bool
(<&&>) = liftA2 (&&)
{-# INLINE (<&&>) #-}
