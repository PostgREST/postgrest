{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module PostgREST.Utils (
  closureSize,
  recursiveSize,
  recursiveSizeNF
) where

import Control.DeepSeq (NFData, ($!!))
import Control.Monad

import GHC.Exts
import GHC.Exts.Heap           hiding (size)
import GHC.Exts.Heap.Constants (wORD_SIZE)

import System.Mem

import Prelude

-- Code in this module is taken from:
--    https://hackage.haskell.org/package/ghc-datasize-0.2.7

-- | Calculate size of GHC objects in Bytes. Note that an object may not be
--   evaluated yet and only the size of the initial closure is returned.
closureSize :: a -> IO Word
closureSize x = do
  rawWds <- getClosureRawWords x
  return . fromIntegral $ length rawWds * wORD_SIZE

-- | Calculate the recursive size of GHC objects in Bytes. Note that the actual
--   size in memory is calculated, so shared values are only counted once.
--
--   Call with
--   @
--    recursiveSize $! 2
--   @
--   to force evaluation to WHNF before calculating the size.
--
--   Call with
--   @
--    recursiveSize $!! \"foobar\"
--   @
--   ($!! from Control.DeepSeq) to force full evaluation before calculating the
--   size.
--
--   A garbage collection is performed before the size is calculated, because
--   the garbage collector would make heap walks difficult.
--
--   This function works very quickly on small data structures, but can be slow
--   on large and complex ones. If speed is an issue it's probably possible to
--   get the exact size of a small portion of the data structure and then
--   estimate the total size from that.

recursiveSize :: a -> IO Word
recursiveSize x = do
  performGC
  fmap snd $ go ([], 0) $ asBox x
  where go (!vs, !acc) b@(Box y) = do
          isElem <- or <$> mapM (areBoxesEqual b) vs
          if isElem
            then return (vs, acc)
            else do
             size    <- closureSize y
             closure <- getClosureData y
             foldM go (b : vs, acc + size) $ allClosures closure

-- | Calculate the recursive size of GHC objects in Bytes after calling
-- Control.DeepSeq.force on the data structure to force it into Normal Form.
-- Using this function requires that the data structure has an `NFData`
-- typeclass instance.

recursiveSizeNF :: NFData a => a -> IO Word
recursiveSizeNF x = recursiveSize $!! x

-- | Adapted from 'GHC.Exts.Heap.getClosureRaw' which isn't exported.
--
-- This returns the raw words of the closure on the heap. Once back in the
-- Haskell world, the raw words that hold pointers may be outdated after a
-- garbage collector run.
getClosureRawWords :: a -> IO [Word]
getClosureRawWords x = do
    case unpackClosure# x of
        (# _iptr, dat, _pointers #) -> do
            let nelems = I# (sizeofByteArray# dat) `div` wORD_SIZE
                end = nelems - 1
            pure [W# (indexWordArray# dat i) | I# i <- [0.. end] ]
