module HasTypeclass2Spec where

import Test.Hspec

import Control.Monad.Reader
import Control.Concurrent.STM

import HasTypeclass2

main :: IO ()
main = hspec $ do
  describe "modify" $ do
    it "works" $ do
      var <- newTVarIO (1 :: Int)
      runReaderT (modify (+ 2)) var
      res <- readTVarIO var
      res `shouldBe` 3
  describe "logSomething" $ do
    it "works" $ do
      var <- newTVarIO ""
      let logFunc msg = atomically $ modifyTVar var (++ msg)
          msg1 = "Hello "
          msg2 = "World\n"
      runReaderT (logSomething msg1 >> logSomething msg2) logFunc
      res <- readTVarIO var
      res `shouldBe` (msg1 ++ msg2)
