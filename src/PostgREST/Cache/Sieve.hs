{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE StrictData                #-}

module PostgREST.Cache.Sieve (
      Cache
    , cache
    , cacheIO
    , cached
    , delete
    , deleteIO
)
where

import Control.Concurrent.STM
import Control.Monad.Extra    (whenJustM, whileM)
--import           Data.Vector            ((!))
import qualified Focus             as F
import           Protolude         hiding (head)
import qualified StmHamt.SizedHamt as SH

data Node = Head {
        next :: TVar Node,
        prev :: TVar Node
    } |
    Node {
        next    :: TVar Node,
        prev    :: TVar Node,
        visited :: TVar Bool
    } deriving Eq

getVisited :: Node -> STM Bool
getVisited (Head _ _)    = pure True
getVisited Node{visited} = readTVar visited

visit :: Node -> STM ()
visit (Head _ _)    = pure ()
visit Node{visited} = whenM (not <$> readTVar visited) (writeTVar visited True)

clear :: Node -> STM ()
clear (Head _ _)    = pure ()
clear Node{visited} = writeTVar visited False

remove :: Node -> STM ()
remove (Head _ _) = pure ()
remove Node{next=currNext, prev=currPrev} = do
    nextEntry <- readTVar currNext
    prevEntry <- readTVar currPrev
    writeTVar (next prevEntry) nextEntry
    writeTVar (prev nextEntry) prevEntry

data Entry k v = Entry {
    ekey  :: k,
    value :: v,
    node  :: Node
} deriving Eq

data Cache m k v =
    Cache {
        entries          :: SH.SizedHamt (Entry k v),
        finger           :: TVar Node,
        maxSize          :: TVar Int,
        head             :: Node,
        load             :: k -> m v,
        requestListener  :: Bool -> m (),
        evictionListener :: m ()
    }

advance :: TVar Node -> STM ()
advance = modifyTVarM (readTVar . next)
    where
        modifyTVarM f = fmap (>>=) (readTVar >=> f) <*> writeTVar

lookupAndVisit :: Hashable k => SH.SizedHamt (Entry k v) -> k -> STM (Maybe v)
lookupAndVisit entries = traverse visitEntry <=< flip (SH.lookup ekey) entries
    where
        visitEntry Entry{node, value} = visit node $> value

cacheIO :: Hashable k => TVar Int -> (k -> m v) -> (Bool -> m ()) -> m () -> IO (Cache m k v)
cacheIO a b c = atomically . cache a b c

cache :: Hashable k => TVar Int -> (k -> m v) -> (Bool -> m ()) -> m () -> STM (Cache m k v)
cache maxSize load rl el = mdo
    head <- Head <$> newTVar head <*> newTVar head
    entries <- SH.new
    fingerTVar <- newTVar head
    pure $ Cache
        entries
        fingerTVar
        maxSize
        head
        load
        rl
        el

delete :: Hashable k => Cache m k v -> k -> STM ()
delete Cache{entries, finger} k =
    whenJustM (SH.focus F.lookupAndDelete ekey k entries) (removeAndCheckFinger . node)
    where
        removeAndCheckFinger node = do
            remove node
            whenM ((node ==) <$> readTVar finger)
                (advance finger)

deleteIO :: Hashable k => Cache m k v -> k -> IO ()
deleteIO c = atomically . delete c

cached :: (Hashable k, MonadIO m) => Cache m k v -> k -> m v
cached Cache{..} k =
    tryMaybe
        -- Fast path: lookup value, update stats and return the value if found
        (liftIO (atomically lookup) >>= (<$) <*> (requestListener . isJust))
        -- Slow path: load/calculate value and insert it (if still not found)
        (do
            value <- load k
            whileM (not <$> tryInsert value)
            pure value)
    where
        lookup = lookupAndVisit entries k
        tryMaybe f notFound = f >>= maybe notFound pure

        -- perform a single entry eviction and possibly insertion atomically
        -- returning False if could not insert
        -- (either because entry currently pointed by the finger was visited
        --  or because after this entry eviction the cache is still full)
        -- so that other threads don't have to wait when visiting entries.
        -- First check if entry is still not in the cache - this time inside transaction.
        --
        -- Execute evictionListener if an entry was evicted
        tryInsert value = do
            (result, listener) <- (liftIO . atomically) $
                runStateT
                    (ifM (isNothing <$> lift lookup)
                        (insertStep value evictionListener)
                        (pure True))
                    -- empty eviction listener
                    (pure ())
            listener $> result

        insertStep v evictionMarker = do
            currDiff <- liftA2 (-) (lift $ SH.size entries) (max 1 <$> lift (readTVar maxSize))
            if currDiff >= 0 then do
                -- no space in the cache
                -- need to evict an entry
                node <- lift $ readTVar finger
                visited <- lift $ getVisited node
                if visited then
                    -- clear and skip visited entry
                    -- not done yet
                    lift (clear node) *> lift (advance finger) $> False
                else do
                    -- found entry to evict
                    -- increment eviction count
                    put evictionMarker
                    -- remove entry and node
                    -- advance finger
                    lift $ SH.focus F.delete ekey k entries
                    lift (remove node) *> lift (advance finger)
                    if currDiff == 0 then
                        -- now there is space
                        -- insert new entry
                        lift (addEntry v) $> True
                    else
                        -- still no space after removal
                        -- not done
                        pure False
            else
                -- there is space in the cache
                lift (addEntry v) $> True

        addEntry v = do
            oldNeck <- readTVar $ prev head
            newNeck <- Node <$> newTVar head <*> newTVar oldNeck <*> newTVar False
            -- add cache entry
            SH.insert ekey (Entry k v newNeck) entries
            -- update pointers
            writeTVar (next oldNeck) newNeck
            writeTVar (prev head) newNeck
