{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module HasTypeclass2 where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Reader
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

modify :: (MonadReader env m, HasBalance env, MonadIO m) => (Int -> Int) -> m ()
modify f = do
  env <- ask
  -- liftIO $ atomically $ modifyTVar' (getBalance env) f
  liftIO . atomically $ modifyTVar' (getBalance env) f

logSomething :: (MonadReader env m, HasLog env, MonadIO m) => String -> m ()
logSomething msg = do
  env <- ask
  -- liftIO $ getLog env msg
  liftIO . getLog env $ msg

main :: IO ()
main = do
  ref <- newTVarIO 4
  let env = Env
        { envLog = sayString
        , envBalance = ref
        }
  runReaderT
    (concurrently
      (modify (+ 1))
      (logSomething "Increasing account balance"))
    env
  balance <- readTVarIO ref
  sayString $ "Final balance: " ++ show balance
