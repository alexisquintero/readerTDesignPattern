module ConcurrentlyModify where

import Control.Concurrent.Async.Lifted
-- import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.State.Strict


concMod :: IO ()
concMod = execStateT
  (concurrently (modify (+ 1)) (modify (+ 2)))
  4 >>= print
