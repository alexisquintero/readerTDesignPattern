{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module RegainPurity where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Control.Concurrent.STM
import Say

data Env = Env
  { envLog :: !(String -> IO ())
  , envBalance :: !(TVar Int)
  }

class HasLog a where
  getLog :: a -> (String -> IO ())
instance HasLog (String -> IO ()) where
  getLog = id
instance HasLog Env where
  getLog = envLog

class HasBalance a where
  getBalance :: a -> TVar Int
instance HasBalance (TVar Int) where
  getBalance = id
instance HasBalance Env where
  getBalance = envBalance

class Monad m => MonadBalance m where
  modifyBalance :: (Int -> Int) -> m ()
instance (HasBalance env, MonadIO m) => MonadBalance (ReaderT env m) where
  modifyBalance f = do
    env <- ask
    -- liftIO $ atomically $ modifyTVar' (getBalance env) f
    liftIO . atomically $ modifyTVar' (getBalance env) f
instance Monad m => MonadBalance (State.StateT Int m) where
  modifyBalance = State.modify

modify :: MonadBalance m => (Int -> Int) -> m ()
modify f = do
  modifyBalance f

logSomething :: (MonadReader env m, HasLog env, MonadIO m) => String -> m ()
logSomething msg = do
  env <- ask
  liftIO $ getLog env msg
