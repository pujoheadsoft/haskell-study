module Main (main) where

import Named (name, The (the))
import Data.Ord (comparing, Down (Down))
import Sorted (sortBy, mergeBy)

main :: IO ()
main = do
  xs <- readLn :: IO [Int]
  ys <- readLn
  name (comparing Down) $ \gt -> do
    let xs' = sortBy gt xs
        ys' = sortBy gt ys
    print (the (mergeBy gt xs' ys'))
