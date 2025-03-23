{-# OPTIONS_GHC -Wno-type-defaults #-}
module SortedSpec where

import Test.Hspec
import Named
import The
import Data.Ord
import Sorted

spec :: Spec
spec = do
  describe "Sorted" $ do
    it "sort" do
      let
        xs = [4, 2, 7]
      result <- name (comparing Down) $ \gt -> do
        pure $ the (sortBy gt xs)
      result `shouldBe` [7, 4, 2]

    it "should sort and merge" do
      let
        xs = [4, 2, 7]
        ys = [9, 8, 3]

      result <- name (comparing Down) \gt -> do
        let xs' = sortBy gt xs
            ys' = sortBy gt ys
        pure $ the (mergeBy gt xs' ys')
      result `shouldBe` [9, 8, 7, 4, 3, 2]
    
    it "minimum" do
      let
        xs = [4, 2, 7]
      result <- name (comparing Down) \gt -> do
        (pure . minimum_01 . sortBy gt) xs
      result `shouldBe` Just 7
