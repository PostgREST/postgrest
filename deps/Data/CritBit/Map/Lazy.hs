-- |
-- Module      :  Data.CritBit.Map.Lazy
-- Copyright   :  (c) Bryan O'Sullivan 2013
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- A crit-bit tree that does not evaluate its values by default.
--
-- For every /n/ key-value pairs stored, a crit-bit tree uses /n/-1
-- internal nodes, for a total of 2/n/-1 internal nodes and leaves.
module Data.CritBit.Map.Lazy
    (
    -- * Types
      CritBitKey(..)
    , CritBit
    , module Data.CritBit.Tree
    ) where

import Data.CritBit.Tree
import Data.CritBit.Types.Internal
