{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import Lib
import Options (Options (..), parseOptions)

main :: IO ()
main = run =<< parseOptions

run :: Options -> IO ()
run options = do
  putStrLn $ "User ID: " ++ userId options
  putStrLn $ "Output Path: " ++ outputPath options