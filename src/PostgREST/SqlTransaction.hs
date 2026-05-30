{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeAbstractions           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module PostgREST.SqlTransaction
  ( SqlTransaction
  , SqlCondemn
  , Condemnable
  , HasTimingsQueryLabel(..)
  , QueryTimings
  , TransactionConstraint
  , SqlTx
  , LockSpec(..)
  , SqlBreakpoint(..)
  , TimingSpec(..)
  , SqlTiming
  , TransactionKind(..)
  , condemn
  , sql
  , statement
  , runSteppedTransaction
  , runTimed
  ) where

import Protolude

import qualified Control.Monad.Writer.Strict as Writer
import           Data.Functor.Contravariant  ((>$<))
import qualified Data.List                   as L
import qualified Data.Text                   as T
import qualified Hasql.Decoders              as HD
import qualified Hasql.Encoders              as HE
import qualified Hasql.Statement             as SQL
import qualified Hasql.Transaction           as SQL

type QueryTimings = [(ByteString, Text)]

class TransactionKind k where
  type LabelConstraint k (label :: k) :: Constraint
  type TransactionCapability k (m :: Type -> Type) :: Constraint
  type TransactionCapability k m = ()

type TransactionConstraint k m = (SqlTransaction k m, TransactionCapability k m)

class (Monad m, TransactionKind k) => SqlTransaction k (m :: Type -> Type) where
  runSql :: forall (label :: k). LabelConstraint k label => ByteString -> m ()
  runStatement :: forall (label :: k) a b. LabelConstraint k label => a -> SQL.Statement a b -> m b

type SqlTx k a = forall m. TransactionConstraint k m => m a

class SqlCondemn (m :: Type -> Type) where
  condemn :: m ()

class (SqlTransaction k m, SqlCondemn m) => Condemnable k m
instance (SqlTransaction k m, SqlCondemn m) => Condemnable k m

sql :: forall {k} (label :: k) m. (SqlTransaction k m, LabelConstraint k label) => ByteString -> m ()
sql = runSql @k @m @label

statement :: forall {k} (label :: k) m a b. (SqlTransaction k m, LabelConstraint k label) => a -> SQL.Statement a b -> m b
statement = runStatement @k @m @label

instance TransactionKind k => SqlTransaction k SQL.Transaction where
  runSql = SQL.sql
  runStatement = SQL.statement

instance SqlCondemn SQL.Transaction where
  condemn = SQL.condemn

instance (Monad m, SqlCondemn m, Writer.MonadTrans t) => SqlCondemn (t m) where
  condemn = lift condemn

newtype WithLock m a = WithLock (ReaderT Int32 m a)
  deriving newtype (Functor, Applicative, Monad, Writer.MonadTrans)

data LockSpec = NoLock | Lock Nat
data TimingSpec = NoTiming | Timing Symbol

type SqlBreakpoint :: forall {k}. k -> Constraint
class SqlLock (BreakpointLock label) => SqlBreakpoint (label :: k) where
  type BreakpointLock label :: LockSpec

type SqlLock :: LockSpec -> Constraint
class SqlLock lock where
  lockNext :: forall k (label :: k) m a. (SqlTransaction k m, LabelConstraint k label) => m a -> WithLock m a

instance KnownNat lock => SqlLock (Lock lock) where
  lockNext @_ @label @m tx = WithLock $ do
    lockId <- ask
    lift $
      statement @label @m
        (lockId, fromIntegral $ natVal (Proxy @lock))
        ( SQL.Statement
            "SELECT pg_advisory_xact_lock($1, $2)"
            ((fst >$< param HE.int4) <> (snd >$< param HE.int2))
            HD.noResult
            False
        )
      *> tx

instance SqlLock NoLock where
  lockNext = lift

instance SqlLock lock => SqlBreakpoint (step lock whatever) where
  type BreakpointLock (step lock whatever) = lock

instance forall k m. (SqlTransaction k m, forall (label :: k). LabelConstraint k label => SqlBreakpoint label) => SqlTransaction k (WithLock m) where
  runSql @label query =
    lockNext @(BreakpointLock label) @k @label @m $ sql @label @m query

  runStatement @label params stmt =
    lockNext @(BreakpointLock label) @k @label @m $ statement @label @m params stmt

runSteppedTransaction :: forall k m a. (TransactionKind k, TransactionConstraint k m, TransactionConstraint k (WithLock m), forall (label :: k). LabelConstraint k label => SqlBreakpoint label) => Maybe Int32 -> SqlTx k a -> m a
runSteppedTransaction l tx = maybe tx (stepped tx) l
  where
    stepped (WithLock m) = runReaderT m

newtype GucTimed k m a = GucTimed (Writer.WriterT [ByteString] m a)
  deriving newtype (Functor, Applicative, Monad, Writer.MonadTrans)

class SqlTiming (label :: k) where
  timeTransactionForLabel :: forall m a. (SqlTransaction k m, LabelConstraint k label) => m a -> GucTimed k m a

instance SqlTiming (step lock NoTiming) where
  timeTransactionForLabel = lift

instance KnownSymbol guc => SqlTiming (step lock (Timing guc)) where
  timeTransactionForLabel @m tx =
    GucTimed $ Writer.WriterT (pure ((), [gucName])) *> timedTx
    where
      GucTimed timedTx = lift $ sFrag gucName *> tx <* eFrag gucName
      gucName = encodeUtf8 . T.pack $ symbolVal (Proxy @guc)
      sFrag name = sql @(step lock (Timing guc)) @m $ "select set_config('pgrst." <> name <> "', clock_timestamp()::text, true)"
      eFrag name = sql @(step lock (Timing guc)) @m $ "select set_config('pgrst." <> name <> "', (clock_timestamp() - current_setting('pgrst." <> name <> "', false)::timestamptz)::text, true)"

instance forall k m. (SqlTransaction k m, forall (label :: k). LabelConstraint k label => SqlTiming label) => SqlTransaction k (GucTimed k m) where
  runSql @label query =
    timeTransactionForLabel @k @label $ sql @label @m query

  runStatement @label params stmt =
    timeTransactionForLabel @k @label $ statement @label @m params stmt

class HasTimingsQueryLabel k where
  type TimingsQueryLabel k :: k

runTimed :: forall k m a. (TransactionKind k, HasTimingsQueryLabel k, TransactionConstraint k m, TransactionConstraint k (GucTimed k m), LabelConstraint k (TimingsQueryLabel k), forall (label :: k). LabelConstraint k label => SqlTiming label) => Bool -> SqlTx k a -> m (a, Maybe QueryTimings)
runTimed isDebug tx =
  if isDebug then timed tx else (, mempty) <$> tx
  where
    timed :: SqlTransaction k m => GucTimed k m a -> m (a, Maybe QueryTimings)
    timed (GucTimed m) = do
      (result, labels) <- Writer.runWriterT m
      (result,) . Just <$>
        statement @(TimingsQueryLabel k) @m
          (decodeUtf8 <$> L.nub labels)
          ( SQL.Statement
              ( "SELECT name, extract('milliseconds' from duration)::text " <>
                "FROM (SELECT name, nullif(current_setting('pgrst.' || name, true), '')::interval AS duration " <>
                "FROM unnest($1::text[]) AS timing(name)) pgrst_timings " <>
                "WHERE duration IS NOT NULL"
              )
              (param . HE.foldableArray . HE.nonNullable $ HE.text)
              (HD.rowList $ ((,) . encodeUtf8 <$> HD.column (HD.nonNullable HD.text)) <*> HD.column (HD.nonNullable HD.text))
              True
          )

param :: HE.Value a -> HE.Params a
param = HE.param . HE.nonNullable
