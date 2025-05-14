{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ImpredicativeTypes #-}

module PostgREST.Cache.Sieve (
      AccessStats(..)
    , Cache
    , cache
    , cacheIO
    , cached
    , delete
    , reset
    , resetIO
    , deleteIO
    , accessStats
    , accessStatsIO
    , evictionsCount
    , evictionsCountIO
)
where

import Protolude hiding (head)
import qualified StmHamt.SizedHamt as SH
import qualified Focus as F
import Control.Monad.Extra (whenJustM)
import qualified Data.Vector as V
import Data.Vector ((!))
import Control.Concurrent.STM
import GHC.Conc (numCapabilities)

data Node = Node {
    visited :: STM Bool,
    visit :: STM (),
    clear :: STM (),
    remove :: STM (),
    next :: TVar Node,
    prev :: TVar Node
}

data Entry k v = Entry {
    ekey :: k,
    value :: v,
    node :: Node
}

data Cache m k v =
    Cache {
        entries :: SH.SizedHamt (Entry k v),
        maxSize :: TVar Int,
        head :: Node,
        finger :: STM Node,
        load :: k -> m v,
        lookupAndVisit :: k -> STM (Maybe v),

        advanceFinger :: STM (),
        reset :: STM (),

        accessStatsVector :: V.Vector (TVar AccessStats),
        evictions :: TVar Int64
    }

data AccessStats = AccessStats {
    requests :: Int64,
    hits :: Int64
}

instance Semigroup AccessStats where
    (AccessStats r1 h1) <> (AccessStats r2 h2) = AccessStats (r1 + r2) (h1 + h2)
instance Monoid AccessStats where
    mempty = AccessStats 0 0

cacheIO :: Hashable k => TVar Int -> (k -> m v) -> IO (Cache m k v)
cacheIO maxSize = atomically . cache maxSize

cache :: Hashable k => TVar Int -> (k -> m v) -> STM (Cache m k v)
cache maxSize load = mdo
    let noop = pure ()
        advanceFinger = modifyTVarM fingerTVar (readTVar . next)
        reset = SH.reset entries *> writeTVar fingerTVar head
        lookupAndVisit = traverse visitEntry <=< flip (SH.lookup ekey) entries
    head <- Node (pure True) noop advanceFinger advanceFinger <$> newTVar head <*> newTVar head
    entries <- SH.new
    fingerTVar <- newTVar head
    cache <- Cache
        entries
        maxSize
        head
        (readTVar fingerTVar)
        load
        lookupAndVisit
        advanceFinger
        reset <$>
        V.replicateM numCapabilities (newTVar $ AccessStats 0 0) <*>
        newTVar 0
    pure cache
    where
        modifyTVarM v f = readTVar v >>= f >>= writeTVar v

        visitEntry Entry{node, value} = visit node $> value

delete :: Hashable k => Cache m k v -> k -> STM ()
delete Cache{entries} k = whenJustM (SH.lookup ekey k entries) (remove . node)

deleteIO :: Hashable k => Cache m k v -> k -> IO ()
deleteIO c = atomically . delete c

resetIO :: Cache m k v -> IO ()
resetIO = atomically . reset

accessStats' :: Monoid (f AccessStats) => (forall a. TVar a -> f a) -> Cache m k v -> f AccessStats
accessStats' readVal = foldMap readVal . accessStatsVector

accessStats :: Cache m k v -> STM AccessStats
accessStats = accessStats' readTVar

accessStatsIO :: Cache m k v -> IO AccessStats
accessStatsIO = accessStats' readTVarIO

evictionsCount :: Cache m k v -> STM Int64
evictionsCount = readTVar . evictions

evictionsCountIO :: Cache m k v -> IO Int64
evictionsCountIO = atomically . evictionsCount

cached :: (Hashable k, MonadIO m) => Cache m k v -> k -> m v
cached Cache{..} k =
    tryMaybe
        -- Fast path: lookup value, update stats and return the value if found
        (liftIO $ atomically $ lookupAndVisit k >>= (<$) <*> updateStats)
        -- Slow path: load/calculate value and insert it (if still not found)
        (load k >>= untilInserted . tryInsert)
    where
        tryMaybe f notFound = f >>= maybe notFound pure

        updateStats result = do
            let statsTVar = accessStatsVector ! (hash k `mod` length accessStatsVector)
            modifyTVar statsTVar $ \AccessStats{..} ->
                AccessStats (requests + 1) (if isJust result then hits + 1 else hits)

        untilInserted x = x >>= maybe (untilInserted x) pure

        -- perform a single entry eviction and possibly insertion atomically
        -- returning Nothing if could not insert
        -- (either because entry currently pointed by the finger was visited
        --  or because after this entry eviction the cache is still full)
        -- so that other threads don't have to wait when visiting entries.
        -- First check if entry is still not in the cache - this time inside transaction.
        --
        -- Careful not to use Alternative instance for STM
        -- (lookupAndVisit k <|> insertStep v) would be wrong
        tryInsert = liftIO . atomically . liftA2 (<|>) (lookupAndVisit k) . insertStep

        insertStep v = do
            currDiff <- liftA2 (-) (SH.size entries) (max 1 <$> readTVar maxSize)
            if currDiff >= 0 then do
                -- no space in the cache
                -- need to evict an entry
                node <- finger
                shouldLeaveEntry <- visited node
                if shouldLeaveEntry then
                    -- clear and skip visited entry
                    -- not done yet
                    clear node $> empty
                else do
                    -- found entry to evict
                    remove node
                    if currDiff == 0 then
                        -- now there is space
                        -- insert new entry
                        -- equivalent point-free (fmap ($>) addEntry <*> pure) v
                        addEntry v $> pure v
                    else
                        -- still no space after removal
                        -- not done
                        pure empty
            else
                -- there is space in the cache
                addEntry v $> pure v

        addEntry v = do
            oldNeck <- readTVar $ prev head
            nextTVar <- newTVar head
            prevTVar <- newTVar oldNeck
            visitedTVar <- newTVar False
            let
                removeEntry = do
                    nextEntry <- readTVar nextTVar
                    prevEntry <- readTVar prevTVar
                    writeTVar (next prevEntry) nextEntry
                    writeTVar (prev nextEntry) prevEntry
                    SH.focus F.delete ekey k entries
                    modifyTVar evictions (+ 1)
                newNeck = Node
                            (readTVar visitedTVar)
                            (writeTVar visitedTVar True)
                            -- both clear and remove advance the finger
                            (writeTVar visitedTVar False *> advanceFinger)
                            (removeEntry *> advanceFinger)
                            nextTVar
                            prevTVar
                newEntry = Entry k v newNeck
            -- add cache entry
            SH.insert ekey newEntry entries
            -- update pointers
            writeTVar (next oldNeck) newNeck
            writeTVar (prev head) newNeck
