module Hw1.Part1Spec where

import           Hw1
import           Test.Hspec

spec :: Spec
spec = do
    it "order3" $ do
        order3 (5, 2, 10) `shouldBe` (2, 5, 10)
        order3 (3, 2, 1) `shouldBe` (1, 2, 3)
        order3 (1, 2, 3) `shouldBe` (1, 2, 3)
        order3 (1, 3, 2) `shouldBe` (1, 2, 3)
        order3 (-1, -3, -2) `shouldBe` (-3, -2, -1)
        order3 (0, 0, 0) `shouldBe` (0, 0, 0)
        order3 (True, False, False) `shouldBe` (False, False, True)

    it "smartReplicate" $ do
        smartReplicate [1, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]
        smartReplicate [0, 2, 0] `shouldBe` [2, 2]
        smartReplicate [2, 1, 4] `shouldBe` [2, 2, 1, 4, 4, 4, 4]
        smartReplicate [5] `shouldBe` [5, 5, 5, 5, 5]
        smartReplicate [] `shouldBe` []

    it "contains" $ do
        contains 3 [[1 .. 5], [2, 0], [3, 4]]
            `shouldBe` [[1, 2, 3, 4, 5], [3, 4]]
        contains 0 [] `shouldBe` []
        contains 3 [[1 .. 3], [1, 2], [3, 4]] `shouldBe` [[1, 2, 3], [3, 4]]
        contains 0 [[1, 2], [2], [0], [-1, 0, 1]] `shouldBe` [[0], [-1, 0, 1]]

    it "stringSum" $ do
        stringSum "1 1" `shouldBe` 2
        stringSum "100\n\t-3" `shouldBe` 97
        stringSum "1 2 3" `shouldBe` 6
        stringSum "1 -2" `shouldBe` -1
        stringSum "1 2 3 4 -4 -3 -2 -1" `shouldBe` 0
        stringSum "\n1\t\n3   555  -1\n\n\n-5 123\t\n\t\n\t\n321 -4 -40"
            `shouldBe` 953
