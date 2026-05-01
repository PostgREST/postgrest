{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Protolude (
  -- * Base functions
  module Base,
  identity,
  pass,
#if !MIN_VERSION_base(4,8,0)
  (&),
  scanl',
#endif
  -- * Function functions
  module Function,
  applyN,
  -- * List functions
  module List,
  map,
  uncons,
  unsnoc,
  -- * Data Structures
  module DataStructures,
  -- * Show functions
  module Show,
  show,
  print,
  -- * Bool functions
  module Bool,
  -- * Monad functions
  module Monad,
  liftIO1,
  liftIO2,
  -- * Functor functions
  module Functor,
  -- * Either functions
  module Either,
  -- * Applicative functions
  module Applicative,
  guarded,
  guardedA,
  -- * String conversion
  module ConvertText,
  -- * Debug functions
  module Debug,

  -- * Panic functions
  module Panic,
  -- * Exception functions
  module Exception,
  Protolude.throwIO,
  Protolude.throwTo,
  -- * Semiring functions
  module Semiring,

  -- * String functions
  module String,
  -- * Safe functions
  module Safe,
  -- * Eq functions
  module Eq,
  -- * Ord functions
  module Ord,
  -- * Traversable functions
  module Traversable,
  -- * Foldable functions
  module Foldable,
  -- * Semigroup functions
#if MIN_VERSION_base(4,9,0)
  module Semigroup,
#endif
  -- * Monoid functions
  module Monoid,
  -- * Bifunctor functions
  module Bifunctor,
  -- * Bifunctor functions
  module Hashable,

  -- * Deepseq functions
  module DeepSeq,

  -- * Tuple functions
  module Tuple,

  module Typeable,

#if MIN_VERSION_base(4,7,0)
  -- * Typelevel programming
  module Typelevel,
#endif

  -- * Monads
  module Fail,
  module State,
  module Reader,
  module Except,
  module Trans,
  module ST,
  module STM,

  -- * Integers
  module Int,
  module Bits,

  -- * Complex functions
  module Complex,

  -- * Char functions
  module Char,

  -- * Maybe functions
  module Maybe,

  -- * Generics functions
  module Generics,

  -- * ByteString functions
  module ByteString,
  LByteString,

  -- * Text functions
  module Text,
  LText,

  -- * Read functions
  module Read,
  readMaybe,
  readEither,

  -- * System functions
  module System,
  die,

  -- * Concurrency functions
  module Concurrency,

  -- * Foreign functions
  module Foreign,
) where

-- Protolude module exports.
import Protolude.Debug as Debug
import Protolude.List as List
import Protolude.Show as Show
import Protolude.Bool as Bool
import Protolude.Monad as Monad
import Protolude.Functor as Functor
import Protolude.Either as Either
import Protolude.Applicative as Applicative
import Protolude.ConvertText as ConvertText
import Protolude.Panic as Panic
import Protolude.Exceptions as Exception
import Protolude.Semiring as Semiring
import qualified Protolude.Conv as Conv

import Protolude.Base as Base hiding (
    putStr           -- Overriden by Show.putStr
  , putStrLn         -- Overriden by Show.putStrLn
  , print            -- Overriden by Protolude.print
  , show             -- Overriden by Protolude.show
  , showFloat        -- Custom Show instances deprecated.
  , showList         -- Custom Show instances deprecated.
  , showSigned       -- Custom Show instances deprecated.
  , showSignedFloat  -- Custom Show instances deprecated.
  , showsPrec        -- Custom Show instances deprecated.
  )
import qualified Protolude.Base as PBase

-- Used for 'show', not exported.
import Data.String (String)
import Data.String as String (IsString)

-- Maybe'ized version of partial functions
import Protolude.Safe as Safe (
    headMay
  , headDef
  , initMay
  , initDef
  , initSafe
  , tailMay
  , tailDef
  , tailSafe
  , lastDef
  , lastMay
  , foldr1May
  , foldl1May
  , foldl1May'
  , maximumMay
  , minimumMay
  , maximumDef
  , minimumDef
  , atMay
  , atDef
  )

-- Applicatives
import Control.Applicative as Applicative (
    Applicative(..)
  , Alternative(..)
  , Const(Const,getConst)
  , ZipList(ZipList,getZipList)
  , (<**>)
  , liftA
  , liftA2
  , liftA3
  , optional
  )

-- Base typeclasses
import Data.Eq as Eq (
    Eq(..)
  )
import Data.Ord as Ord (
    Ord(..)
  , Ordering(LT,EQ,GT)
  , Down(Down)
  , comparing
  )
import Data.Traversable as Traversable
import Data.Foldable as Foldable (
    Foldable,
    fold,
    foldMap,
    foldr,
    foldr',
    foldl,
    foldl',
    toList,
#if MIN_VERSION_base(4,8,0)
    null,
    length,
#endif
    elem,
    maximum,
    minimum,
    foldrM,
    foldlM,
    traverse_,
    for_,
    mapM_,
    forM_,
    sequence_,
    sequenceA_,
    asum,
    msum,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    notElem,
    find,
  )
import Data.Functor.Identity as Functor (
    Identity(Identity, runIdentity)
  )

#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty as List (
    NonEmpty((:|))
  , nonEmpty
  )
import Data.Semigroup as Semigroup (
    Semigroup(sconcat, stimes)
  , WrappedMonoid
  , diff
  , cycle1
  , stimesMonoid
  , stimesIdempotent
  , stimesIdempotentMonoid
  , mtimesDefault
  )
#endif

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,16,0)
import Data.Semigroup as Semigroup (
    Option(..)
  , option
  )
#endif

import Data.Monoid as Monoid

#if !MIN_VERSION_base(4,8,0)
import Protolude.Bifunctor as Bifunctor (Bifunctor(bimap, first, second))
#else
import Data.Bifunctor as Bifunctor (Bifunctor(bimap, first, second))
#endif

-- Deepseq
import Control.DeepSeq as DeepSeq (
    NFData(..)
  , ($!!)
  , deepseq
  , force
  )

-- Data structures
import Data.Tuple as Tuple (
    fst
  , snd
  , curry
  , uncurry
  , swap
  )

import Data.List as List (
    splitAt
  , break
  , intercalate
  , isPrefixOf
  , isInfixOf
  , isSuffixOf
  , drop
  , filter
  , reverse
  , replicate
  , take
  , sortBy
  , sort
  , intersperse
  , transpose
  , subsequences
  , permutations
  , scanl
#if MIN_VERSION_base(4,8,0)
  , scanl'
#endif
  , scanr
  , iterate
  , repeat
  , cycle
  , unfoldr
  , takeWhile
  , dropWhile
  , group
  , inits
  , tails
  , zipWith
  , zip
  , unzip
  , genericLength
  , genericTake
  , genericDrop
  , genericSplitAt
  , genericReplicate
  )

#if !MIN_VERSION_base(4,8,0)
-- These imports are required for the scanl' rewrite rules
import GHC.Exts (build)
import Data.List (tail)
#endif

-- Hashing
import Data.Hashable as Hashable (
    Hashable
  , hash
  , hashWithSalt
  , hashUsing
  )

import Data.Map as DataStructures (Map)
import Data.Set as DataStructures (Set)
import Data.Sequence as DataStructures (Seq)
import Data.IntMap as DataStructures (IntMap)
import Data.IntSet as DataStructures (IntSet)

import Data.Typeable as Typeable (
    TypeRep
  , Typeable
  , typeOf
  , cast
  , gcast
#if MIN_VERSION_base(4,7,0)
  , typeRep
  , eqT
#endif
  )

#if MIN_VERSION_base(4,7,0)
import Data.Proxy as Typelevel (
    Proxy(..)
  )

import Data.Type.Coercion as Typelevel (
    Coercion(..)
  , coerceWith
  , repr
  )

import Data.Type.Equality as Typelevel (
    (:~:)(..)
  , type (==)
  , sym
  , trans
  , castWith
  , gcastWith
  )

#endif

#if MIN_VERSION_base(4,8,0)
import Data.Void as Typelevel (
    Void
  , absurd
  , vacuous
  )
#endif

import Control.Monad.Fail as Fail (
    MonadFail
  )

-- Monad transformers
import Control.Monad.State as State (
    MonadState
  , State
  , StateT(StateT)
  , put
  , get
  , gets
  , modify
  , state
  , withState

  , runState
  , execState
  , evalState

  , runStateT
  , execStateT
  , evalStateT
  )

import Control.Monad.Reader as Reader (
    MonadReader
  , Reader
  , ReaderT(ReaderT)
  , ask
  , asks
  , local
  , reader
  , runReader
  , runReaderT
  )

import Control.Monad.Trans.Except as Except (
    throwE
  , catchE
  )

import Control.Monad.Except as Except (
    MonadError
  , Except
  , ExceptT(ExceptT)
  , throwError
  , catchError
  , runExcept
  , runExceptT
  , mapExcept
  , mapExceptT
  , withExcept
  , withExceptT
  )

import Control.Monad.Trans as Trans (
    MonadIO
  , lift
  , liftIO
  )

-- Base types
import Data.Int as Int (
    Int
  , Int8
  , Int16
  , Int32
  , Int64
  )
import Data.Bits as Bits (
  Bits,
  (.&.),
  (.|.),
  xor,
  complement,
  shift,
  rotate,
#if MIN_VERSION_base(4,7,0)
  zeroBits,
#endif
  bit,
  setBit,
  clearBit,
  complementBit,
  testBit,
#if MIN_VERSION_base(4,7,0)
  bitSizeMaybe,
#endif
  bitSize,
  isSigned,
  shiftL,
  shiftR,
  rotateL,
  rotateR,
  popCount,
#if MIN_VERSION_base(4,7,0)
  FiniteBits,
  finiteBitSize,
  bitDefault,
  testBitDefault,
  popCountDefault,
#endif
#if MIN_VERSION_base(4,8,0)
  toIntegralSized,
  countLeadingZeros,
  countTrailingZeros,
#endif
  )
import Data.Word as Bits (
    Word
  , Word8
  , Word16
  , Word32
  , Word64
#if MIN_VERSION_base(4,7,0)
  , byteSwap16
  , byteSwap32
  , byteSwap64
#endif
  )

import Data.Either as Either (
    Either(Left,Right)
  , either
  , lefts
  , rights
  , partitionEithers
#if MIN_VERSION_base(4,7,0)
  , isLeft
  , isRight
#endif
  )

import Data.Complex as Complex (
    Complex((:+))
  , realPart
  , imagPart
  , mkPolar
  , cis
  , polar
  , magnitude
  , phase
  , conjugate
  )
import Data.Char as Char (
    Char
  , ord
  , chr
  , digitToInt
  , intToDigit
  , toUpper
  , toLower
  , toTitle
  , isAscii
  , isLetter
  , isDigit
  , isHexDigit
  , isPrint
  , isAlpha
  , isAlphaNum
  , isUpper
  , isLower
  , isSpace
  , isControl
  )
import Data.Bool as Bool (
  Bool(True, False),
  (&&),
  (||),
  not,
  otherwise
  )
import Data.Maybe as Maybe (
    Maybe(Nothing, Just)
  , maybe
  , isJust
  , isNothing
  , fromMaybe
  , listToMaybe
  , maybeToList
  , catMaybes
  , mapMaybe
  )

import Data.Function as Function (
    const
  , (.)
  , ($)
  , flip
  , fix
  , on
#if MIN_VERSION_base(4,8,0)
  , (&)
#endif
  )

-- Genericss
import GHC.Generics as Generics (
    Generic(..)
  , Generic1
  , Rep
  , K1(..)
  , M1(..)
  , U1(..)
  , V1
  , D1
  , C1
  , S1
  , (:+:)(..)
  , (:*:)(..)
  , (:.:)(..)
  , Rec0
  , Constructor(..)
  , Datatype(..)
  , Selector(..)
  , Fixity(..)
  , Associativity(..)
#if ( __GLASGOW_HASKELL__ >= 800 )
  , Meta(..)
  , FixityI(..)
  , URec
#endif
  )

-- ByteString
import qualified Data.ByteString.Lazy
import Data.ByteString as ByteString (ByteString)

-- Text
import Data.Text as Text (
    Text
  , lines
  , words
  , unlines
  , unwords
  )
import qualified Data.Text.Lazy

import Data.Text.IO as Text (
    getLine
  , getContents
  , interact
  , readFile
  , writeFile
  , appendFile
  )

import Data.Text.Lazy as Text (
    toStrict
  , fromStrict
  )

import Data.Text.Encoding as Text (
    encodeUtf8
  , decodeUtf8
  , decodeUtf8'
  , decodeUtf8With
  )

import Data.Text.Encoding.Error as Text (
    OnDecodeError
  , OnError
  , UnicodeException
  , lenientDecode
  , strictDecode
  , ignore
  , replace
  )

-- IO
import System.Environment as System (getArgs)
import qualified System.Exit
import System.Exit as System (
    ExitCode(..)
  , exitWith
  , exitFailure
  , exitSuccess
  )
import System.IO as System (
    Handle
  , FilePath
  , IOMode(..)
  , stdin
  , stdout
  , stderr
  , withFile
  , openFile
  )

-- ST
import Control.Monad.ST as ST (
    ST
  , runST
  , fixST
  )

-- Concurrency and Parallelism
import Control.Exception as Exception (
    Exception,
    toException,
    fromException,
#if MIN_VERSION_base(4,8,0)
    displayException,
#endif
    SomeException(SomeException)
  , IOException
  , ArithException(
    Overflow,
    Underflow,
    LossOfPrecision,
    DivideByZero,
    Denormal,
    RatioZeroDenominator
    )
  , ArrayException(IndexOutOfBounds, UndefinedElement)
  , AssertionFailed(AssertionFailed)
#if MIN_VERSION_base(4,7,0)
  , SomeAsyncException(SomeAsyncException)
  , asyncExceptionToException
  , asyncExceptionFromException
#endif
  , AsyncException(StackOverflow, HeapOverflow, ThreadKilled, UserInterrupt)
  , NonTermination(NonTermination)
  , NestedAtomically(NestedAtomically)
  , BlockedIndefinitelyOnMVar(BlockedIndefinitelyOnMVar)
  , BlockedIndefinitelyOnSTM(BlockedIndefinitelyOnSTM)
#if MIN_VERSION_base(4,8,0)
  , AllocationLimitExceeded(AllocationLimitExceeded)
#endif
#if MIN_VERSION_base(4,10,0)
  , CompactionFailed(CompactionFailed)
#endif
  , Deadlock(Deadlock)
  , NoMethodError(NoMethodError)
  , PatternMatchFail(PatternMatchFail)
  , RecConError(RecConError)
  , RecSelError(RecSelError)
  , RecUpdError(RecUpdError)
#if MIN_VERSION_base(4,9,0)
  , ErrorCall(ErrorCall, ErrorCallWithLocation)
#else
  , ErrorCall(ErrorCall)
#endif
#if MIN_VERSION_base(4,9,0)
  , TypeError(TypeError)
#endif
  , ioError
  , catch
  , catches
  , Handler(Handler)
  , catchJust
  , handle
  , handleJust
  , try
  , tryJust
  , evaluate
  , mapException
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_
  , MaskingState(..)
  , getMaskingState
#if MIN_VERSION_base(4,9,0)
  , interruptible
#endif
  , allowInterrupt
  , bracket
  , bracket_
  , bracketOnError
  , finally
  , onException
  )
import qualified Control.Exception as PException

import Control.Monad.STM as STM (
    STM
  , atomically
#if !(MIN_VERSION_stm(2,5,0))
  , always
  , alwaysSucceeds
#endif
  , retry
  , orElse
  , check
  , throwSTM
  , catchSTM
  )

import Control.Concurrent.MVar as Concurrency (
    MVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , isEmptyMVar
  , withMVar
#if MIN_VERSION_base(4,7,0)
  , withMVarMasked
#endif
  , modifyMVar_
  , modifyMVar
  , modifyMVarMasked_
  , modifyMVarMasked
#if MIN_VERSION_base(4,7,0)
  , tryReadMVar
  , mkWeakMVar
#endif
  , addMVarFinalizer
  )
import Control.Concurrent.Chan as Concurrency (
    Chan
  , newChan
  , writeChan
  , readChan
  , dupChan
  , getChanContents
  , writeList2Chan
  )
import Control.Concurrent.QSem as Concurrency (
    QSem
  , newQSem
  , waitQSem
  , signalQSem
  )
import Control.Concurrent.QSemN as Concurrency (
    QSemN
  , newQSemN
  , waitQSemN
  , signalQSemN
  )
import Control.Concurrent as Concurrency (
    ThreadId
  , forkIO
  , forkFinally
  , forkIOWithUnmask
  , killThread
  , forkOn
  , forkOnWithUnmask
  , getNumCapabilities
  , setNumCapabilities
  , threadCapability
  , yield
  , threadDelay
  , threadWaitRead
  , threadWaitWrite
#if MIN_VERSION_base(4,7,0)
  , threadWaitReadSTM
  , threadWaitWriteSTM
#endif
  , rtsSupportsBoundThreads
  , forkOS
#if MIN_VERSION_base(4,9,0)
  , forkOSWithUnmask
#endif
  , isCurrentThreadBound
  , runInBoundThread
  , runInUnboundThread
  , mkWeakThreadId
  , myThreadId
  )
import Control.Concurrent.Async as Concurrency (
    Async(..)
  , Concurrently(..)
  , async
  , asyncBound
  , asyncOn
  , withAsync
  , withAsyncBound
  , withAsyncOn
  , wait
  , poll
  , waitCatch
  , cancel
  , cancelWith
  , asyncThreadId
  , waitAny
  , waitAnyCatch
  , waitAnyCancel
  , waitAnyCatchCancel
  , waitEither
  , waitEitherCatch
  , waitEitherCancel
  , waitEitherCatchCancel
  , waitEither_
  , waitBoth
  , link
  , link2
  , race
  , race_
  , concurrently
  )

import Foreign.Ptr as Foreign (IntPtr, WordPtr)
import Foreign.Storable as Foreign (Storable)
import Foreign.StablePtr as Foreign (StablePtr)

-- Read instances hiding unsafe builtins (read)
import qualified Text.Read as Read
import Text.Read as Read (
    Read
  , reads
  )

-- Type synonymss for lazy texts
type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString


#if !MIN_VERSION_base(4,8,0)
infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x
#endif

-- | The identity function, returns the give value unchanged.
identity :: a -> a
identity x = x

map :: Functor.Functor f => (a -> b) -> f a -> f b
map = Functor.fmap

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

unsnoc :: [x] -> Maybe ([x],x)
unsnoc = Foldable.foldr go Nothing
  where
    go x mxs = Just (case mxs of
       Nothing -> ([], x)
       Just (xs, e) -> (x:xs, e))

-- | Apply a function n times to a given value
applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) identity (List.replicate n f)

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
--
-- >>> readMaybe ("123" :: Text) :: Maybe Int
-- Just 123
--
-- >>> readMaybe ("hello" :: Text) :: Maybe Int
-- Nothing
readMaybe :: (Read b, Conv.StringConv a String) => a -> Maybe b
readMaybe = Read.readMaybe . Conv.toS

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
--
-- >>> readEither "123" :: Either Text Int
-- Right 123
--
-- >>> readEither "hello" :: Either Text Int
-- Left "Prelude.read: no parse"
readEither :: (Read a, Conv.StringConv String e, Conv.StringConv e String) => e -> Either e a
readEither = first Conv.toS . Read.readEither . Conv.toS

-- | The print function outputs a value of any printable type to the standard
-- output device. Printable types are those that are instances of class Show;
-- print converts values to strings for output using the show operation and adds
-- a newline.
print :: (Trans.MonadIO m, PBase.Show a) => a -> m ()
print = liftIO . PBase.print

-- | Lifted throwIO
throwIO :: (Trans.MonadIO m, Exception e) => e -> m a
throwIO = liftIO . PException.throwIO

-- | Lifted throwTo
throwTo :: (Trans.MonadIO m, Exception e) => ThreadId -> e -> m ()
throwTo tid e = liftIO (PException.throwTo tid e)

-- | Do nothing returning unit inside applicative.
pass :: Applicative f => f ()
pass = pure ()

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = Bool.bool empty (pure x) (p x)

guardedA :: (Functor.Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
guardedA p x = Bool.bool empty (pure x) `Functor.fmap` p x

-- | Lift an 'IO' operation with 1 argument into another monad
liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO

-- | Lift an 'IO' operation with 2 arguments into another monad
liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 = ((.).(.)) liftIO

show :: (Show a, Conv.StringConv String b) => a -> b
show x = Conv.toS (PBase.show x)
{-# SPECIALIZE show :: Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show  a => a -> String  #-}

#if MIN_VERSION_base(4,8,0)
-- | Terminate main process with failure
die :: Text -> IO a
die err = System.Exit.die (ConvertText.toS err)
#else
-- | Terminate main process with failure
die :: Text -> IO a
die err = hPutStrLn stderr err >> exitFailure
#endif

#if !MIN_VERSION_base(4,8,0)
-- This is a literal copy of the implementation in GHC.List in base-4.10.1.0.

-- | A strictly accumulating version of 'scanl'
{-# NOINLINE [1] scanl' #-}
scanl'           :: (b -> a -> b) -> b -> [a] -> [b]
scanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f !q ls    = q : (case ls of
                            []   -> []
                            x:xs -> scanlGo' f (f q x) xs)

{-# RULES
"scanl'"  [~1] forall f a bs . scanl' f a bs =
  build (\c n -> a `c` foldr (scanlFB' f c) (flipSeqScanl' n) bs a)
"scanlList'" [1] forall f a bs .
    foldr (scanlFB' f (:)) (flipSeqScanl' []) bs a = tail (scanl' f a bs)
 #-}

{-# INLINE [0] scanlFB' #-}
scanlFB' :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB' f c = \b g -> \x -> let !b' = f x b in b' `c` g b'

{-# INLINE [0] flipSeqScanl' #-}
flipSeqScanl' :: a -> b -> a
flipSeqScanl' a !_b = a
#endif
