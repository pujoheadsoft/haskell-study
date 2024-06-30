module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec 
spec = do 
  describe "テストケース" $ do 
    it "somefuncが返ってくること" $ do
      someFunc `shouldBe` "someFunc"
