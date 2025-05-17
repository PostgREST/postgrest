{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE StrictData                 #-}

module PostgREST.Cache.Sieve (
      Cache
    , cache
    , cacheIO
    , cached
    , delete
    , deleteIO
)
where

import           Control.Concurrent.STM
import           Control.Monad.Extra    (whenJustM, whileM)
import qualified Focus                  as F
import           Protolude              hiding (head)
import qualified StmHamt.SizedHamt      as SH

newtype EntryKey k = EntryKey k deriving (Eq, Hashable)

data Node k = Head {
        next :: TVar (Node k),
        prev :: TVar (Node k)
    } |
    Node {
        next     :: TVar (Node k),
        prev     :: TVar (Node k),
        visited  :: TVar Bool,

        entryKey :: EntryKey k
    } deriving Eq

data Entry k v = Entry {
    ekey  :: EntryKey k,
    value :: v,
    node  :: Node k
} deriving Eq

data Cache m k v =
    Cache {
        entries          :: SH.SizedHamt (Entry k v),
        finger           :: TVar (Node k),
        maxSize          :: TVar Int,
        head             :: Node k,
        load             :: k -> m v,
        requestListener  :: Bool -> m (),
        evictionListener :: m ()
    }

getVisited :: Node k -> STM Bool
getVisited Head{}        = pure True
getVisited Node{visited} = readTVar visited

visit :: Node k -> STM ()
visit Head{}    = pure ()
visit Node{visited} = whenM (not <$> readTVar visited) (writeTVar visited True)

clear :: Node k -> STM ()
clear Head{}        = pure ()
clear Node{visited} = writeTVar visited False

remove :: Node k -> STM ()
remove (Head _ _) = pure ()
remove Node{next=currNext, prev=currPrev} = do
    nextEntry <- readTVar currNext
    prevEntry <- readTVar currPrev
    writeTVar (next prevEntry) nextEntry
    writeTVar (prev nextEntry) prevEntry

removeEntry :: (Hashable k) => Node k -> SH.SizedHamt (Entry k v) -> STM ()
removeEntry Head{} _ = pure ()
removeEntry Node{entryKey} entries = SH.focus F.delete ekey entryKey entries

advance :: TVar (Node k) -> STM ()
advance = modifyTVarM (readTVar . next)
    where
        modifyTVarM f = fmap (>>=) (readTVar >=> f) <*> writeTVar

lookupAndVisit :: Hashable k => SH.SizedHamt (Entry k v) -> EntryKey k -> STM (Maybe v)
lookupAndVisit entries key = SH.focus focus ekey key entries
    where
        focus = F.Focus
            (pure (Nothing, F.Leave))
            (\entry -> visit (node entry) $> (Just $ value entry, F.Leave))

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
    whenJustM (SH.focus F.lookupAndDelete ekey (EntryKey k) entries) (removeAndCheckFinger . node)
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
        (liftIO (atomically lookup) >>= (<$) <*> requestListener . isJust)
        -- Slow path: load/calculate value and insert it (if still not found)
        (do
            value <- load k
            whileM (not <$> tryInsert value)
            pure value)
    where
        lookup = lookupAndVisit entries $ EntryKey k
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
            (result, evicted) <- liftIO . atomically $
                SH.focus (insFocus value) ekey (EntryKey k) entries
            evicted $> result

        insFocus v = F.Focus (do
            (hasSpace, evicted) <- runStateT evictionStep $ pure ()
            if hasSpace then do
                entry <- addEntry v
                -- done, maybe evicted, insert entry
                pure ((True, evicted), F.Set entry)
            else
                -- not done, maybe evicted, don't modify entries
                pure ((False, evicted), F.Leave))
            (\entry -> do
                visit $ node entry
                -- done, no evictions, don't modify entries
                pure ((True, pure ()), F.Leave))

        -- if the cache is full precoesses a single node
        -- removing it if it is marked as unvisited
        -- or clearing visited mark
        -- returns True if there is space in the cache
        -- puts evictionListener in state if an entry was evicted
        evictionStep = do
            currDiff <- lift $ liftA2 (-) (SH.size entries) (max 1 <$> readTVar maxSize)
            if currDiff >= 0 then do
                -- no space in the cache
                -- need to evict an entry
                node <- lift $ readTVar finger
                visited <- lift $ getVisited node
                if visited then
                    -- clear and skip visited entry
                    -- not done yet
                    lift $ clear node *> advance finger $> False
                else do
                    -- found entry to evict
                    -- set evictionListener
                    put evictionListener
                    -- remove entry and node
                    -- advance finger
                    lift $ do
                        removeEntry node entries
                        remove node
                        advance finger
                    if currDiff == 0 then
                        -- now there is space in the cache
                        pure True
                    else
                        -- still no space after removal
                        -- not done
                        pure False
            else
                -- there is space in the cache
                pure True

        addEntry v = do
            oldNeck <- readTVar $ prev head
            newNeck <- Node <$> newTVar head <*> newTVar oldNeck <*> newTVar False <*> pure (EntryKey k)
            -- update pointers
            writeTVar (next oldNeck) newNeck
            writeTVar (prev head) newNeck
            -- return HAMT entry
            pure $ Entry (EntryKey k) v newNeck
