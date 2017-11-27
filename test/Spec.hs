module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "test suite" $ do
    it "does nothing and exits" $ do
      'a' `shouldBe` 'a'
