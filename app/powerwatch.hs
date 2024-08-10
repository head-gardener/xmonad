module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import PowerWatch
import Data.List (intercalate)

main :: IO ()
main = do
  bats <- watch
  unless (null bats) $ putStrLn $ "Watching batteries: " ++ intercalate ", " bats
  forever (threadDelay maxBound)
