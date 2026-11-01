-- This tries to emulate the problem on https://github.com/PostgREST/postgrest/issues/5367,
-- but just focusing on the hasql pool.
module Main where

import Control.Concurrent (getNumCapabilities, newChan, newEmptyMVar, putMVar, readChan, readMVar, writeChan)
import Control.Concurrent.Async (concurrently_, replicateConcurrently_)
import Control.Exception (bracket)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)
import Prelude

import Hasql.Connection.Setting qualified as Connection
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Config
import Hasql.Session qualified as Session

main :: IO ()
main = bracket (Pool.acquire settings) Pool.release $ \pool -> do
  -- init
  warmPool poolSize pool

  -- bench
  startNSecs <- getMonotonicTimeNSec
  let noop = pure ()
   in bench pool vus usesPerVU noop
  endNSecs <- getMonotonicTimeNSec

  -- report
  capabilities <- getNumCapabilities
  let
    totalUses = vus * usesPerVU
    benchSeconds = fromIntegral (endNSecs - startNSecs) / 1e9 :: Double
    usesPerSecond = fromIntegral totalUses / benchSeconds
   in
    printf "- N%d: %.0f pool uses/s (%d uses in %.3fs)\n" capabilities usesPerSecond totalUses benchSeconds
  where
    -- The run on https://github.com/PostgREST/postgrest/issues/5367 has a db-pool of 150,
    -- but our local pg has max_connections = 100 so using 150 fails.
    -- Use 90 because using 100 fails too since pg has some reserved connections.
    poolSize = 90

    -- Matches the VUs (virtual users) on https://github.com/PostgREST/postgrest/issues/5367
    -- Having this higher than pool connections tests contention on the pool.
    vus = 400

    -- The run on https://github.com/PostgREST/postgrest/issues/5367 runs for 60 seconds,
    -- to make this run faster just use a constant number of uses.
    -- This would be more like using "constant arrival rate" on the benchmark.
    usesPerVU = 2000

    -- Uses PostgREST defaults for the config.
    settings =
      Config.settings
        [ Config.size poolSize,
          Config.acquisitionTimeout 10,
          Config.agingTimeout 1800,
          Config.idlenessTimeout 30,
          -- Read connection from PG* env vars provided by the Nix wrapper.
          Config.staticConnectionSettings [Connection.connection ""]
        ]

-- Simulate the work the pool does on https://github.com/PostgREST/postgrest/issues/5367,
-- concurrent workers acquire and return pool connections.
bench :: Pool.Pool -> Int -> Int -> Session.Session () -> IO ()
bench pool workers uses session =
  -- The workers run concurrently
  replicateConcurrently_ workers $
    -- but the work is done sequentially, one pool use at a time and waiting for the connection to return to the pool
    replicateM_ uses $
      usePool pool session

-- Ensures all pool connections are initialized and borrowed.
warmPool :: Int -> Pool.Pool -> IO ()
warmPool poolSize pool = do
  borrowed <- newChan
  filled <- newEmptyMVar
  -- do it concurrently so both actions run and wait for each other to finish
  concurrently_
    ( replicateConcurrently_ poolSize $ usePool pool $ liftIO $ do
        writeChan borrowed () -- signal one borrowed pool connection
        readMVar filled -- hold the connection until the pool size is filled
    )
    ( do
        replicateM_ poolSize (readChan borrowed) -- wait until every pool connection is borrowed
        putMVar filled () -- the pool is filled, finish the warm
    )

usePool :: Pool.Pool -> Session.Session a -> IO a
usePool pool session = Pool.use pool session >>= either (ioError . userError . show) pure -- aborts on errors
