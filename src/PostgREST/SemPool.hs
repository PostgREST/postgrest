{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}

module PostgREST.SemPool (Pool, PoolError(..), pool, use, release) where

import           Control.Arrow ((&&&))
import           Control.Monad
import           Data.Functor
import           Data.IORef
import           Data.Sequence hiding (reverse, splitAt)
import qualified Data.Sequence as Seq
import           Protolude     hiding (QSem, newQSem, signalQSem,
                                waitQSem, withState)

class Traversable p => Queue p where
    prepend :: a -> p a -> p a
    append :: p a -> a -> p a
    split :: Int -> p a -> (p a, p a)
    headTails :: p a -> Maybe (a, p a)
    len :: p a -> Int

instance Queue [] where
    prepend = (:)
    append xs x = xs ++ [x]
    split = splitAt
    headTails = uncons
    len = Protolude.length

instance Queue Seq where
    prepend = (<|)
    append = (|>)
    split = Seq.splitAt
    headTails = (\case
        EmptyL -> Nothing
        x :< xs -> Just (x, xs)) . viewl
    len = Data.Sequence.length

data StrictPair a b = StrictPair !a !b
    deriving Functor

type Q a = Seq a

type PS a = (IORef Bool, Q a)
newtype PoolState a = PoolState (MVar (PS a))

modifyState :: PoolState a -> (PS a -> IO (PS a, b)) -> IO b
modifyState (PoolState mv) = modifyMVar mv

modifyState_ :: PoolState a -> (PS a -> IO (PS a)) -> IO ()
modifyState_ (PoolState mv) = modifyMVar_ mv

withState :: PoolState a -> (IORef Bool -> Q a -> IO (Q a, b)) -> IO b
withState s f = modifyState s $ \(blockReuse, idle) -> fmap (first (blockReuse,)) (f blockReuse idle)

withIdle :: PoolState a -> (Q a -> IO (Q a, b)) -> IO b
withIdle s = withState s . const

data Pool e a =
    Pool {
        waitSem          :: (forall b. IO b -> IO b) -> IO (),
        signalSem        :: IO (),
        withAvailableSem :: forall b. (Int -> IO b) -> IO b,
        acquireTimeout   :: Int,
        initResource     :: IO (Either e a),
        validateResource :: a -> IO (Maybe e),
        destroyResource  :: a -> IO (),
        poolState        :: PoolState a
    }

data ResourceHandle = forall e a. ResourceHandle (Pool e a) (IORef Bool) a

data PoolError e =
    AcquireTimeout | ResourceError e deriving Show

data CancelTimer = CancelTimer deriving (Show, Exception)

emptyPoolState :: IO (PS a)
emptyPoolState = (,mempty) <$> newIORef False

pool :: Nat -> Int -> IO (Either e a) -> (a -> IO (Maybe e)) -> (a -> IO ()) -> IO (Pool e a)
pool size timeout init validate destroy = newQSem size >>= \sem ->
    Pool
        (\unmask -> unmask $ waitQSem sem)
        (signalQSem sem)
        (withAvailableQSem sem)
        timeout init validate destroy <$> (emptyPoolState >>= fmap PoolState . newMVar)

release :: Pool e a -> IO ()
release Pool{..} =
    uninterruptibleMask_ $ do
        outstanding <- modifyState poolState $ \(blockReuse, idle) -> do
            -- inside modifyState so safe
            writeIORef blockReuse True
            (, idle) <$> emptyPoolState
        forkAllUnmasked destroyResource outstanding

-- call this with with exceptions disabled
cleanup :: Pool a b -> IO ()
cleanup Pool{..} = do
    outstanding <- withAvailableSem $ \available -> withIdle poolState (pure . \idle -> if len idle > available then split available idle else (idle, mempty))
    forkAllUnmasked destroyResource outstanding

forkAllUnmasked :: Traversable t => (a -> IO ()) -> t a -> IO ()
forkAllUnmasked f =
    traverse_ (forkUnmasked . f)
    where
        forkUnmasked m = forkIOWithUnmask $ \unmask -> unmask m

acquireWithTimeout :: Pool e a -> IO (Either (PoolError e) (a, ResourceHandle))
acquireWithTimeout p@Pool{..} = do
    result <- newEmptyMVar
    -- start timer
    timerThreadId <- forkIO $
        (threadDelay (acquireTimeout * 1000) *>
        void (tryPutMVar result (Right $ Left AcquireTimeout))) `catch` \CancelTimer -> pure ()
    let
        -- this is one big critical section so we mask all interrupts/exceptions
        -- unmasking:
        -- 1) waiting on poolSemaphore
        -- 2) invocation of initResource (after establishing exception ResourceHandler)
        -- 3) cancellation of timer thread
        -- TODO: (modifyMVar idle) is blocking and possibly should be interruptible
        --   OTOH it is always fast as it is simple list head manipulation so maybe can stay like this
        acquire :: (forall a. IO a -> IO a) -> IO ()
        acquire unmask = do
            let getOrCreate = withState poolState $
                    \mv idle -> pure . maybe (idle, (mv,) <$> unmask initResource) (fmap (prepare . (mv,)) . swap) $ headTails idle
                -- TODO logging of validation failures
                prepare (mv, r) = unmask (validateResource r) >>= maybe (pure (mv, Right r)) (const $ join getOrCreate)

            waitSem unmask
            (blockReuse, acquired) <- fmap (first ResourceError) <$> join getOrCreate `onException` signalSem
            ifM (tryPutMVar result $ Right $ (identity &&& ResourceHandle p blockReuse) <$> acquired)
                (do
                    -- result is set but if failed to acquire resource
                    -- need to release the semaphore
                    when (isLeft acquired) signalSem
                    -- Cancel the timer in a separate thread in case
                    -- timer thread is masked
                    void $ forkIOWithUnmask $ \unmask2 -> unmask2 $ throwTo timerThreadId CancelTimer
                )
                (do
                    -- we are late so clean-up
                    -- return acquired resource to the pool
                    -- add it at the end of the list
                    traverse_ (\a -> modifyState_ poolState (pure . fmap (`append` a))) acquired
                    -- release semaphore
                    signalSem
                    -- make sure we don't keep outstanding resources
                    -- it might happen if we were late and returned fresh resource to idle
                    cleanup p
                )
    -- start acquire and rethrow any exceptions from it
    void $ uninterruptibleMask_ $ forkIOWithUnmask $
        \unmask -> acquire unmask `catchAll` (void . tryPutMVar result . Left)
    takeMVar result >>= either throwIO pure
    where
        catchAll = catch @SomeException

returnToPool :: ResourceHandle -> IO ()
returnToPool (ResourceHandle p@Pool{..} blockReuse a) =
    uninterruptibleMask $ \unmask -> do
        postSignalAction <- withIdle poolState (\idle ->
            -- inside withIdle so safe
            ifM (readIORef blockReuse)
                -- return resource to idle list
                -- need to cleanup because there
                -- might be some outstanding
                -- resources from timeouted aquisitions
                (pure (prepend a idle, cleanup p))
                -- reuse is blocked so leave poolState as is and destroy resource
                (pure (idle, unmask $ destroyResource a)))
        signalSem
        postSignalAction

use :: Pool e a -> (a -> IO b) -> IO (Either (PoolError e) b)
use p f =
    unmaskingBracket
        (acquireWithTimeout p)
        (traverse (returnToPool . snd))
        (traverse (f . fst))
    where
    unmaskingBracket before after thing =
        mask $ \unmask -> do
            a <- unmask before
            r <- unmask (thing a) `onException` unmask (after a)
            unmask (after a) $> r

---------- The below copy of QSem code is needed to implement this function
withAvailableQSem :: QSem -> (Int -> IO a) -> IO a
withAvailableQSem (QSem mv) f = modifyMVar mv $ \t@(a, _) -> (t,) <$> f a

------------- QSem implementation copied from base
------------- Done to expose availableQSem function that provides number of available permits

------------- EDIT: changed implementation so that it uses Data.Seq instead of two lists

-- | 'QSem' is a quantity semaphore in which the resource is acquired
-- and released in units of one. It provides guaranteed FIFO ordering
-- for satisfying blocked `waitQSem` calls.
--
-- The pattern
--
-- > bracket_ waitQSem signalQSem (...)
--
-- is safe; it never loses a unit of the resource.
--
newtype QSem = QSem (MVar (Int, Seq (MVar ())))

-- The semaphore state (i, xs, ys):
--
--   i is the current resource value
--
--   (xs,ys) is the queue of blocked threads, where the queue is
--           given by xs ++ reverse ys.  We can enqueue new blocked threads
--           by consing onto ys, and dequeue by removing from the head of xs.
--
-- A blocked thread is represented by an empty (MVar ()).  To unblock
-- the thread, we put () into the MVar.
--
-- A thread can dequeue itself by also putting () into the MVar, which
-- it must do if it receives an exception while blocked in waitQSem.
-- This means that when unblocking a thread in signalQSem we must
-- first check whether the MVar is already full; the MVar lock on the
-- semaphore itself resolves race conditions between signalQSem and a
-- thread attempting to dequeue itself.

-- |Build a new 'QSem' with a supplied initial quantity.
--  The initial quantity must be at least 0.
newQSem :: Nat -> IO QSem
newQSem initial = do
      sem <- newMVar (fromIntegral initial, mempty)
      return (QSem sem)

-- |Wait for a unit to become available.
waitQSem :: QSem -> IO ()
waitQSem (QSem m) =
  mask_ $ do
    (i,b1) <- takeMVar m
    if i == 0
       then do
         b <- newEmptyMVar
         putMVar m (i, b1 |> b)
         waitOn b
       else do
         let !z = i-1
         putMVar m (z, b1)
         return ()
  where
    waitOn b = takeMVar b `onException`
                uninterruptibleMask_ (do -- Note [signal uninterruptible]
                   (i,b1) <- takeMVar m
                   r <- tryTakeMVar b
                   r' <- if isJust r
                            then signal (i,b1)
                            else do putMVar b (); return (i,b1)
                   putMVar m r')

-- |Signal that a unit of the 'QSem' is available.
signalQSem :: QSem -> IO ()
signalQSem (QSem m) =
  uninterruptibleMask_ $ do -- Note [signal uninterruptible]
    r <- takeMVar m
    r' <- signal r
    putMVar m r'

-- Note [signal uninterruptible]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--   If we have
--
--      bracket waitQSem signalQSem (...)
--
--   and an exception arrives at the signalQSem, then we must not lose
--   the resource.  The signalQSem is masked by bracket, but taking
--   the MVar might block, and so it would be interruptible.  Hence we
--   need an uninterruptibleMask here.
--
--   This isn't ideal: during high contention, some threads won't be
--   interruptible.  The QSemSTM implementation has better behaviour
--   here, but it performs much worse than this one in some
--   benchmarks.

signal :: (Int,Seq (MVar ())) -> IO (Int,Seq (MVar ()))
signal (i,a1) =
 if i == 0
   then loop a1
   else let !z = i+1 in return (z, a1)
 where
   loop Empty = return (1, mempty)
   loop (b :<| bs) =
     ifM (tryPutMVar b ())
        (pure (0, bs))
        (loop bs)
