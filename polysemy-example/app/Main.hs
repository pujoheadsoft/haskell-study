module Main (main) where

import TeletypeEffect as T
import ResourceEffect as R

main :: IO ()
main = do
  R.runProgram >>= either
    (\_ -> putStr "")
    (\_ -> putStr "")

