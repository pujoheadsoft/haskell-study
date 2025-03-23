{-# OPTIONS_GHC -Wno-type-defaults #-}
module JMapSpec where

import Test.Hspec

import qualified Data.Map as M
import JMap
import Prelude hiding (lookup)

spec :: Spec
spec = do
  describe "JMap" do
    it "withMap, member, lookup" do
      let m = M.fromList [(1, "one"), (2, "two")]
      withMap m $ \table -> do
        case member 1 table of
          Nothing -> fail "member 1"
          Just key -> do
            lookup key table `shouldBe` "one"
        case member 2 table of
          Nothing -> fail "member 2"
          Just key -> do
            lookup key table `shouldBe` "two"