module Feature.MetricsPoolAvailableSpec where

import qualified Hasql.Pool.Observation as SQL
import           PostgREST.Metrics      (PoolAvailableState (..),
                                         emptyPoolAvailableState,
                                         poolAvailableGaugeValue,
                                         stepPoolAvailable)
import           Protolude
import           Test.Hspec             (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "pgrst_db_pool_available" $ do
    it "does not decrement on Connecting -> Terminated" $ do
      let state0 = emptyPoolAvailableState (Just 10) :: PoolAvailableState Text
          state1 =
            applyEvents state0
              [ ("a", SQL.ConnectingConnectionStatus),
                ("a", terminated)
              ]
      poolAvailableCount state1 `shouldBe` 0

    it "does not double-decrement on Ready -> InUse -> Terminated" $ do
      let state0 = emptyPoolAvailableState (Just 10) :: PoolAvailableState Text
          counts =
            countsAfter
              state0
              [ ("a", SQL.ReadyForUseConnectionStatus),
                ("a", SQL.InUseConnectionStatus),
                ("a", terminated)
              ]
      counts `shouldBe` [1, 0, 0]

    it "ignores duplicate ReadyForUse observations" $ do
      let state0 = emptyPoolAvailableState (Just 10) :: PoolAvailableState Text
          counts =
            countsAfter
              state0
              [ ("a", SQL.ReadyForUseConnectionStatus),
                ("a", SQL.ReadyForUseConnectionStatus)
              ]
      counts `shouldBe` [1, 1]

    it "ignores duplicate Terminated observations for unknown connections" $ do
      let state0 = emptyPoolAvailableState (Just 10) :: PoolAvailableState Text
          state1 = applyEvents state0 [("a", terminated), ("a", terminated)]
      poolAvailableCount state1 `shouldBe` 0

    it "tracks multiple connections independently" $ do
      let state0 = emptyPoolAvailableState (Just 10) :: PoolAvailableState Text
          counts =
            countsAfter
              state0
              [ ("a", SQL.ReadyForUseConnectionStatus),
                ("b", SQL.ReadyForUseConnectionStatus),
                ("a", SQL.InUseConnectionStatus)
              ]
      counts `shouldBe` [1, 2, 1]

    it "treats out-of-order observations as last-write-wins" $ do
      let state0 = emptyPoolAvailableState (Just 10) :: PoolAvailableState Text
          counts =
            countsAfter
              state0
              [ ("a", terminated),
                ("a", SQL.ReadyForUseConnectionStatus),
                ("a", SQL.InUseConnectionStatus)
              ]
      counts `shouldBe` [0, 1, 0]

    it "clamps gauge values to pool_max" $ do
      let state0 = emptyPoolAvailableState (Just 1) :: PoolAvailableState Text
          state1 =
            applyEvents state0
              [ ("a", SQL.ReadyForUseConnectionStatus),
                ("b", SQL.ReadyForUseConnectionStatus)
              ]
      poolAvailableCount state1 `shouldBe` 2
      poolAvailableGaugeValue state1 `shouldBe` 1

  where
    terminated = SQL.TerminatedConnectionStatus SQL.ReleaseConnectionTerminationReason
    applyEvents = foldl' stepPoolAvailable
    countsAfter st events = poolAvailableCount <$> drop 1 (scanl stepPoolAvailable st events)
