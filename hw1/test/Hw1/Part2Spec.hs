module Hw1.Part2Spec where

import           Data.List.NonEmpty             ( fromList )
import           Hw1
import           Test.Hspec

spec :: Spec
spec = do
    it "removeAt" $ do
        removeAt 2 [0, 1, 2] `shouldBe` (Just 2, [0, 1])
        removeAt 100 "abcd" `shouldBe` (Nothing, "abcd")
        removeAt (-1) "abcd" `shouldBe` (Nothing, "abcd")
        removeAt 1 "abc" `shouldBe` (Just 'b', "ac")
        removeAt 0 [5, 4, 3] `shouldBe` (Just 5, [4, 3])
        removeAt 50 [0 .. 99] `shouldBe` (Just 50, [0 .. 49] ++ [51 .. 99])

    it "mergeSort" $ do
        mergeSort [1, 4, 6] `shouldBe` [1, 4, 6]
        mergeSort [0, -2] `shouldBe` [-2, 0]
        mergeSort [1, 0, 4, -2, 6] `shouldBe` [-2, 0, 1, 4, 6]
        mergeSort [1, 2, 3] `shouldBe` [1, 2, 3]
        mergeSort [9, 8, 7, 6, 5] `shouldBe` [5, 6, 7, 8, 9]
