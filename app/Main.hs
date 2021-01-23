module Main where

import ConcurrentlyModify
import ConcurrentlyTvar

main :: IO ()
main = do
  putStr "Unsafe concurrent modify: "
  concMod
  putStr "Safe concurrent modify: "
  concTvar
