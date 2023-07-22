-- | Basic thread effects.
--
-- @since 0.1
module Effectful.Thread
  ( -- * Threads

    -- ** Effect
    ThreadEffect (..),
    microsleep,

    -- *** Handlers
    runThreadIO,

    -- ** Misc
    sleep,

    -- * QSem

    -- ** Effect
    QSemEffect (..),
    newQSem,
    waitQSem,
    signalQSem,
    newQSemN,
    waitQSemN,
    signalQSemN,

    -- *** Handlers
    runQSemIO,

    -- * Re-exports
    Natural,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as QSem
import Control.Concurrent.QSemN (QSemN)
import Control.Concurrent.QSemN qualified as QSemN
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (interpret, send)
import GHC.Natural (Natural)

-- | Effects for general threads.
--
-- @since 0.1
data ThreadEffect :: Effect where
  Microsleep :: Natural -> ThreadEffect m ()

-- | @since 0.1
type instance DispatchOf ThreadEffect = Dynamic

-- | Runs 'ThreadEffect' in 'IO'.
--
-- @since 0.1
runThreadIO ::
  ( IOE :> es
  ) =>
  Eff (ThreadEffect : es) a ->
  Eff es a
runThreadIO = interpret $ \_ -> \case
  Microsleep n -> liftIO $ for_ (natToInts n) threadDelay

-- | @since 0.1
microsleep :: (ThreadEffect :> es) => Natural -> Eff es ()
microsleep = send . Microsleep

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (ThreadEffect :> es) => Natural -> Eff es ()
sleep = microsleep . (* 1_000_000)

natToInts :: Natural -> [Int]
natToInts n
  | n > maxIntAsNat = maxInt : natToInts (n - maxIntAsNat)
  | otherwise = [n2i n]
  where
    maxInt :: Int
    maxInt = maxBound
    maxIntAsNat :: Natural
    maxIntAsNat = i2n maxInt

n2i :: Natural -> Int
n2i = fromIntegral

i2n :: Int -> Natural
i2n = fromIntegral

-- | Effects for semaphores.
--
-- @since 0.1
data QSemEffect :: Effect where
  NewQSem :: Int -> QSemEffect m QSem
  WaitQSem :: QSem -> QSemEffect m ()
  SignalQSem :: QSem -> QSemEffect m ()
  NewQSemN :: Int -> QSemEffect m QSemN
  WaitQSemN :: QSemN -> Int -> QSemEffect m ()
  SignalQSemN :: QSemN -> Int -> QSemEffect m ()

-- | @since 0.1
type instance DispatchOf QSemEffect = Dynamic

-- | Runs 'ThreadEffect' in 'IO'.
--
-- @since 0.1
runQSemIO ::
  ( IOE :> es
  ) =>
  Eff (QSemEffect : es) a ->
  Eff es a
runQSemIO = interpret $ \_ -> \case
  NewQSem i -> liftIO $ QSem.newQSem i
  WaitQSem q -> liftIO $ QSem.waitQSem q
  SignalQSem q -> liftIO $ QSem.signalQSem q
  NewQSemN i -> liftIO $ QSemN.newQSemN i
  WaitQSemN q i -> liftIO $ QSemN.waitQSemN q i
  SignalQSemN q i -> liftIO $ QSemN.signalQSemN q i

-- | @since 0.1
newQSem :: (QSemEffect :> es) => Int -> Eff es QSem
newQSem = send . NewQSem

-- | @since 0.1
waitQSem :: (QSemEffect :> es) => QSem -> Eff es ()
waitQSem = send . WaitQSem

-- | @since 0.1
signalQSem :: (QSemEffect :> es) => QSem -> Eff es ()
signalQSem = send . SignalQSem

-- | @since 0.1
newQSemN :: (QSemEffect :> es) => Int -> Eff es QSemN
newQSemN = send . NewQSemN

-- | @since 0.1
waitQSemN :: (QSemEffect :> es) => QSemN -> Int -> Eff es ()
waitQSemN q = send . WaitQSemN q

-- | @since 0.1
signalQSemN :: (QSemEffect :> es) => QSemN -> Int -> Eff es ()
signalQSemN q = send . SignalQSemN q
