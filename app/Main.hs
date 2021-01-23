module Main where

import ConcurrentlyModify
import ConcurrentlyTvar
import qualified HasTypeclass1 as HT1
import qualified HasTypeclass2 as HT2

main :: IO ()
main = do
  putStr "Unsafe concurrent modify: "
  concMod
  putStr "Safe concurrent modify: "
  concTvar
  putStrLn "Has typeclass 1"
  HT1.main
  putStrLn "Has typeclass 2"
  HT2.main
