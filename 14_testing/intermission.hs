module Intermission where

import Test.Hspec

infixl 5 ***
(***) :: (Integral a) => a -> a -> a
x *** y =
  go 0 x y
    where
      go acc x 0 = acc
      go acc x y = go (acc + x) x (y - 1)

testMultiplication :: IO ()
testMultiplication = hspec $ do
  describe "***" $ do
    it "123 *** 0 is 0" $ do
      123 *** 0 `shouldBe` 0
    it "123 *** 1 is 123" $ do
      123 *** 1 `shouldBe` 123
    it "123 *** 123 is 15129" $ do
      123 *** 123 `shouldBe` 15129
