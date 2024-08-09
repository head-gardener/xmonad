module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import PowerWatch

main :: IO ()
main = watch >> forever (threadDelay maxBound)
