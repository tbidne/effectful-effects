-- | Functionality common to all annotation frameworks.
--
-- @since 0.1
module Effectful.Exception.Annotation.Common
  ( MonadGlobalException (..),
    ExceptionProxy (..),
  )
where

import Control.Exception (Exception, SomeException)
import Data.Proxy (Proxy)
import Effectful (Eff)
import Effectful.Dispatch.Static (unEff, unsafeEff, unsafeEff_)
import GHC.Conc.Sync qualified as Sync
import GHC.Stack.Types (HasCallStack)

-------------------------------------------------------------------------------
--                           MonadGlobalException                            --
-------------------------------------------------------------------------------

-- | Effect for global exception mechanisms.
--
-- @since 0.1
class (Monad m) => MonadGlobalException m where
  -- | Lifted 'Sync.setUncaughtExceptionHandler'.
  --
  -- @since 0.1
  setUncaughtExceptionHandler :: (HasCallStack) => (SomeException -> m ()) -> m ()

  -- | Lifted 'Sync.getUncaughtExceptionHandler'.
  --
  -- @since 0.1
  getUncaughtExceptionHandler :: (HasCallStack) => m (SomeException -> m ())

-- | @since 0.1
instance MonadGlobalException IO where
  setUncaughtExceptionHandler = Sync.setUncaughtExceptionHandler
  {-# INLINEABLE setUncaughtExceptionHandler #-}

  getUncaughtExceptionHandler = Sync.getUncaughtExceptionHandler
  {-# INLINEABLE getUncaughtExceptionHandler #-}

-- | @since 0.1
instance MonadGlobalException (Eff es) where
  setUncaughtExceptionHandler setter = unsafeEff $ \env ->
    setUncaughtExceptionHandler (\someEx -> unEff (setter someEx) env)
  {-# INLINEABLE setUncaughtExceptionHandler #-}

  getUncaughtExceptionHandler =
    unsafeEff_ $
      getUncaughtExceptionHandler >>= \handler -> pure (unsafeEff_ . handler)
  {-# INLINEABLE getUncaughtExceptionHandler #-}

-- | Proxy for exception types. Used for matching multiple exception types
--
-- @since 0.1
data ExceptionProxy = forall e. (Exception e) => MkExceptionProxy (Proxy e)
