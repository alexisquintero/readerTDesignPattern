{-# LANGUAGE FlexibleContexts #-}

module ConcurrentlyTvar where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Reader
import Control.Concurrent.STM

modify :: (MonadReader (TVar Int) m, MonadIO m) => (Int -> Int) -> m ()
modify f = do
  ref <- ask
  liftIO $ atomically $ modifyTVar' ref f

concTvar :: IO ()
concTvar = do
  ref <- newTVarIO 4
  runReaderT (concurrently (modify (+ 1)) (modify (+ 2))) ref
  readTVarIO ref >>= print
