{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : PostgREST.Cache.Sieve
-- Description : PostgREST cache implementation based on Sieve algorithm.
--
-- This module provides implementation of a mutable cache on Sieve algorithm.
module PostgREST.Cache.Sieve
  ( Cache
  , CacheConfig (..)
  , Discard (..)
  , alwaysValid
  , cache
  , cacheIO
  , cached
  , trackThrashing
  )
where

import Control.Concurrent.STM
import Control.Monad.Extra (whileM)
import Data.Some
import Protolude hiding (elem, head)

import Focus qualified as F
import StmHamt.SizedHamt qualified as SH

-- | A node of a circular doubly linked list. The list starts with a head
-- node, whose element is of type @e False@, followed by the nodes of its
-- elements, of type @e True@.
data ListNode (e :: Bool -> Type) (b :: Bool) = ListNode
  { nextPtr :: NodePtr e,
    prevNextPtrPtr :: NodePtrPtr e,
    elem :: e b
  }

type AnyNode e = Some (ListNode e)

type NodePtr e = TVar (AnyNode e)

type NodePtrPtr e = TVar (NodePtr e)

-- | A new empty list: its head, with an element made from the head's next
-- pointer, links to itself.
newList :: (NodePtr e -> STM (e False)) -> STM (ListNode e False)
newList headElem = mdo
  tail <- newTVar (Some head)
  head <- ListNode tail <$> newTVar tail <*> headElem tail
  pure head

-- | Append an element at the end of the list, returning its node.
append :: ListNode e False -> e True -> STM (ListNode e True)
append head@ListNode{prevNextPtrPtr = neck} e = do
  oldNeckNextPtr <- readTVar neck
  newNeckNextPtr <- newTVar (Some head)
  newNeck <- ListNode newNeckNextPtr <$> newTVar oldNeckNextPtr <*> pure e
  -- update pointers
  writeTVar oldNeckNextPtr (Some newNeck)
  writeTVar neck newNeckNextPtr
  pure newNeck

-- | Remove a node from its list.
unlink :: ListNode e True -> STM ()
unlink ListNode{nextPtr, prevNextPtrPtr = currPrev} = do
  nextNode <- readTVar nextPtr
  withSome nextNode $ \n -> do
    prevNextPtr <- readTVar currPrev
    writeTVar (prevNextPtrPtr n) prevNextPtr
    writeTVar prevNextPtr nextNode

data NodeElem :: Type -> Type -> Bool -> Type where
  Head
    :: { entries :: SH.SizedHamt (HamtEntry k v),
         finger :: NodePtrPtr (NodeElem k v)
       }
    -> NodeElem k v False
  Entry
    :: (Hashable k)
    => { visited :: TVar Bool,
         ekey :: k,
         entryValue :: v
       }
    -> NodeElem k v True

type HamtEntry k v = ListNode (NodeElem k v) True

data Discard m v = Refresh (m ()) | Invalid (m v)

data Cache m k v = (Hashable k, MonadIO m) => Cache (ListNode (NodeElem k v) False) (CacheConfig m k v) (Maybe (Ghosts m))

-- | The hash of a key. Thrashing tracking remembers evicted keys by their
-- hashes, not the keys themselves: an evicted key should release its memory
-- (a key can be large, e.g. a JWT), and a fixed-size hash keeps the memory of
-- tracking bounded by the number of tracked keys, whatever their size.
type KeyHash = Int

-- | The hashes of the most recently evicted keys, oldest first, to detect
-- cache thrashing. A hash is removed when its key is missed.
data Ghosts m = Ghosts
  { ghostsMaxSize :: STM Int,
    thrashingListener :: m (),
    ghostEntries :: SH.SizedHamt (ListNode GhostElem True),
    ghostList :: ListNode GhostElem False
  }

data GhostElem :: Bool -> Type where
  GhostHead :: GhostElem False
  Ghost :: KeyHash -> GhostElem True

data CacheConfig m k v = CacheConfig
  { maxSize :: STM Int,
    load :: k -> m v,
    requestListener :: Bool -> m (),
    evictionListener :: k -> v -> m (),
    validator :: m (k -> v -> Maybe (Discard m v))
  }

alwaysValid :: (Applicative m) => m (k -> v -> Maybe (Discard m v))
alwaysValid = pure (const . const Nothing)

cacheIO :: (Hashable k, MonadIO m) => CacheConfig m k v -> IO (Cache m k v)
cacheIO = atomically . cache

cache :: (Hashable k, MonadIO m) => CacheConfig m k v -> STM (Cache m k v)
cache cacheConfig = do
  head <- newList $ \tail -> Head <$> SH.new <*> newTVar tail
  pure $ Cache head cacheConfig Nothing

-- | Run the listener on a miss of a key that is among the given number of
-- most recently evicted keys not missed since: the miss would have been a hit
-- in a cache larger by that number. Keys removed as invalid are not evicted.
--
-- Only the hashes of evicted keys are kept, so a hash collision may rarely
-- report a miss of a key that was not evicted.
trackThrashing :: STM Int -> m () -> Cache m k v -> STM (Cache m k v)
trackThrashing ghostsMaxSize thrashingListener (Cache head cacheConfig _) = do
  ghostEntries <- SH.new
  ghostList <- newList . const $ pure GhostHead
  pure $ Cache head cacheConfig $ Just Ghosts{..}

cached :: Cache m k v -> k -> m v
cached (Cache head@ListNode{elem = Head{..}} CacheConfig{..} ghosts) k = do
  checkValid <- validator
  tryMaybe
    -- Fast path: lookup value, update stats and return the value if found and valid
    ((liftIO . atomically) (lookup checkValid) >>= notify (requestListener . isJust) >>= notify (flip unless notifyThrashing . isJust) >>= validate)
    -- Slow path: load/calculate value and insert it (if still not found)
    ( do
        value <- load k
        whileM (not <$> tryInsert value)
        pure value
    )
  where
    tryMaybe f notFound = f >>= maybe notFound pure

    notify = ((<$) <*>)

    notifyThrashing =
      for_ ghosts $ \g@Ghosts{thrashingListener} ->
        whenM (liftIO . atomically $ removeGhost (hash k) g) thrashingListener

    validate =
      fmap join
        . traverse
          ( \case
              -- valid value
              (Right v) -> pure $ Just v
              -- refresh value
              (Left (Refresh act)) -> act $> Nothing
              -- discard value and return alt result
              (Left (Invalid res)) -> Just <$> res
          )

    lookup checkValid = SH.focus focus (ekey . elem) k entries
      where
        focus =
          F.Focus
            -- not found
            (pure (Nothing, F.Leave))
            -- found
            -- check entry validity
            ( \e@ListNode{elem = Entry{visited, entryValue}} ->
                maybe
                  -- entry valid
                  (mark visited True $> (Just $ Right entryValue, F.Leave))
                  -- entry invalid
                  -- remove it
                  ((removeEntry e $>) . (,F.Remove) . Just . Left)
                  (checkValid k entryValue)
            )

    mark t b = whenM ((/= b) <$> readTVar t) (writeTVar t b)

    -- perform a single entry eviction and possibly insertion atomically
    -- returning False if could not insert
    -- (either because entry currently pointed by the finger was visited
    --  or because after this entry eviction the cache is still full)
    -- so that other threads don't have to wait when visiting entries.
    -- First check if entry is still not in the cache - this time inside transaction.
    --
    -- Execute evictionListener if an entry was evicted
    tryInsert value = do
      (result, evicted) <- liftIO . atomically $ do
        -- Use SH.focus to performa a single lookup instead of 2
        -- we cannot modify Hamt from inside focus
        -- so if there is any entry to remove
        -- we need to delete it after
        (res, evictedKey) <- SH.focus focus (ekey . elem) k entries
        case evictedKey of
          (Just Entry{ekey = entryKey, entryValue}) -> do
            SH.focus F.delete (ekey . elem) entryKey entries
            traverse_ (addGhost $ hash entryKey) ghosts
            pure (res, evictionListener entryKey entryValue)
          Nothing -> pure (res, pure ())

      evicted $> result
      where
        focus =
          F.Focus
            ( do
                (hasSpace, evictedKey) <- evictionStep
                if hasSpace then do
                  entry <- newLinkedEntry value
                  -- done, maybe evicted, insert entry
                  pure ((True, evictedKey), F.Set entry)
                else
                  -- not done, maybe evicted, don't modify entries
                  pure ((False, evictedKey), F.Leave)
            )
            -- Entry found case
            ( \ListNode{elem = Entry{visited}} -> do
                -- mark as visited
                mark visited True
                -- done, no evictions, don't modify entries
                pure ((True, Nothing), F.Leave)
            )

    -- if the cache is full precoesses a single node
    -- removing it if it is marked as unvisited
    -- or clearing visited mark
    -- returns True if there is space in the cache
    -- puts evictionListener in state if an entry was evicted
    evictionStep = do
      currDiff <- liftA2 (-) (SH.size entries) (max 1 <$> maxSize)
      if currDiff >= 0 then do
        -- no space in the cache
        -- need to evict an entry
        (nextFinger, evictedKey) <- readTVar finger >>= evict
        writeTVar finger nextFinger
        -- return if enough space and evicted key if any
        pure (isJust evictedKey && currDiff == 0, evictedKey)
      else
        -- there is space in the cache
        pure (True, Nothing)

    evict :: NodePtr (NodeElem k v) -> STM (NodePtr (NodeElem k v), Maybe (NodeElem k v True))
    evict =
      readTVar >=> \case
        (Some e@ListNode{nextPtr, prevNextPtrPtr, elem = elem@Entry{visited}}) -> do
          ifM
            (readTVar visited)
            (writeTVar visited False $> (nextPtr, Nothing))
            (unlink e *> fmap (,Just elem) (readTVar prevNextPtrPtr))
        -- skip head
        (Some ListNode{nextPtr, elem = Head{}}) -> evict nextPtr

    newLinkedEntry v = do
      -- return HAMT entry
      append head =<< (Entry <$> newTVar False <*> pure k <*> pure v)

    removeEntry = fmap (*>) unlink <*> adjustFinger

    adjustFinger ListNode{nextPtr, prevNextPtrPtr} =
      whenM ((nextPtr ==) <$> readTVar finger) $
        readTVar prevNextPtrPtr >>= writeTVar finger

-- | Remember the hash of an evicted key, forgetting the oldest ones over the max size.
addGhost :: KeyHash -> Ghosts m -> STM ()
addGhost h g@Ghosts{..} = do
  -- a key evicted again (or a colliding one) keeps only its latest eviction
  void $ removeGhost h g
  ghost <- append ghostList $ Ghost h
  SH.insert ghostHash ghost ghostEntries
  dropOldest
  where
    dropOldest =
      whenM (liftA2 (>) (SH.size ghostEntries) (max 0 <$> ghostsMaxSize)) $
        readTVar (nextPtr ghostList) >>= \case
          Some oldest@ListNode{elem = Ghost oldestHash} -> do
            unlink oldest
            SH.focus F.delete ghostHash oldestHash ghostEntries
            dropOldest
          Some ListNode{elem = GhostHead} -> pass

-- | Forget the hash of a missed key, returning whether it was evicted recently.
removeGhost :: KeyHash -> Ghosts m -> STM Bool
removeGhost h Ghosts{ghostEntries} =
  SH.focus F.lookupAndDelete ghostHash h ghostEntries >>= fmap isJust . traverse unlink

ghostHash :: ListNode GhostElem True -> KeyHash
ghostHash ListNode{elem = Ghost h} = h
