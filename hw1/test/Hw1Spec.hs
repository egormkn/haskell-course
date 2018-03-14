module Hw1Spec where

import Hw1
import Test.Hspec

spec :: Spec
spec = do
  it "order3" $ do
    order3 (3, 2, 1)   `shouldBe` (1, 2, 3)
    order3 (1, 2, 3)   `shouldBe` (1, 2, 3)
    order3 (6, 3, 9)   `shouldBe` (3, 6, 9)
    order3 (9, -1, -8) `shouldBe` (-8, -1, 9)

  it "smartReplicate" $ do
    smartReplicate [1, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]
    smartReplicate [2, 1, 4] `shouldBe` [2, 2, 1, 4, 4, 4, 4]
    smartReplicate [5]       `shouldBe` [5, 5, 5, 5, 5]
    smartReplicate []        `shouldBe` []

  it "contains" $ do
    contains 0 []                             `shouldBe` [] 
    contains 3 [[1..3], [1, 2], [3, 4]]       `shouldBe` [[1, 2, 3], [3, 4]]
    contains 0 [[1, 2], [2], [0], [-1, 0, 1]] `shouldBe` [[0], [-1, 0, 1]]

  it "removeAt" $ do
    removeAt 2 [0, 1, 2]   `shouldBe` (Just 2, [0, 1])
    removeAt 100 "_---"    `shouldBe` (Nothing,"_---")
    removeAt (-1) "_---"   `shouldBe` (Nothing,"_---")
    removeAt 1 "abc"       `shouldBe` (Just 'b', "ac")

  it "stringSum" $ do
    stringSum "1 2 3"               `shouldBe` 6
    stringSum "1 -2"                `shouldBe` -1
    stringSum "1 2 3 4 -4 -3 -2 -1" `shouldBe` 0
    
  it "mergeSort" $ do
    mergeSort [1, 0, 4, -2, 6]   `shouldBe` [-2, 0, 1, 4, 6]
    mergeSort [1, 2, 3]          `shouldBe` [1, 2, 3]
    mergeSort [9, 8, 7, 6, 5]    `shouldBe` [5, 6, 7, 8, 9]
    

    
  
  it "Nat" $ do
    toInteger Z                     `shouldBe` 0
    toInteger (S Z)                 `shouldBe` 1
    toInteger (S (S Z))             `shouldBe` 2
    toInteger (S (S (S Z)))         `shouldBe` 3
    toInteger (S (S (S (S (S Z))))) `shouldBe` 5
    Z           == Z                `shouldBe` True
    S Z         == S (S Z)          `shouldBe` False
    (23 :: Nat) == (11 :: Nat)      `shouldBe` False
    (19 :: Nat) == (19 :: Nat)      `shouldBe` True
