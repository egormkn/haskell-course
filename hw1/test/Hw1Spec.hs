module Hw1Spec where

import Data.List.NonEmpty (fromList)
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

  it "stringSum" $ do
    stringSum "1 2 3"               `shouldBe` 6
    stringSum "1 -2"                `shouldBe` -1
    stringSum "1 2 3 4 -4 -3 -2 -1" `shouldBe` 0

  it "removeAt" $ do
    removeAt 2 [0, 1, 2]   `shouldBe` (Just 2, [0, 1])
    removeAt 100 "abcd"    `shouldBe` (Nothing,"abcd")
    removeAt (-1) "abcd"   `shouldBe` (Nothing,"abcd")
    removeAt 1 "abc"       `shouldBe` (Just 'b', "ac")

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
    (1 :: Nat) == S Z               `shouldBe` True
    (1 :: Nat) == S (S Z)           `shouldBe` False
    (19 :: Nat) == (19 :: Nat)      `shouldBe` True
    (23 :: Nat) == (11 :: Nat)      `shouldBe` False
    (19 :: Nat) + (19 :: Nat)       `shouldBe` (38 :: Nat)
    (19 :: Nat) - (19 :: Nat)       `shouldBe` (0 :: Nat)
    (5 :: Nat) * (5 :: Nat)         `shouldBe` (25 :: Nat)
    (19 :: Nat) <= (19 :: Nat)      `shouldBe` True
    (19 :: Nat) > (5 :: Nat)        `shouldBe` True
    (21 :: Nat) `div` (10 :: Nat)   `shouldBe` (2 :: Nat)
    (21 :: Nat) `mod` (10 :: Nat)   `shouldBe` (1 :: Nat)

  it "Day" $ do
    nextDay Mon                     `shouldBe` Tue
    nextDay Wed                     `shouldBe` Thu
    nextDay Sun                     `shouldBe` Mon
    afterDays 7 Fri                 `shouldBe` Fri
    afterDays 10 Tue                `shouldBe` Fri
    isWeekend Mon                   `shouldBe` False
    isWeekend Wed                   `shouldBe` False
    isWeekend Sun                   `shouldBe` True
    daysToParty Fri                 `shouldBe` 0
    daysToParty Mon                 `shouldBe` 4

  it "City" $ do
    cityPopulation (buildHouse (City Nothing Nothing (Data.List.NonEmpty.fromList [One, Two, Two])) 3) `shouldBe` 8
    buildHouse (buildHouse (City Nothing Nothing (Data.List.NonEmpty.fromList [One, Two, Two])) 3) 4 `shouldBe` (City Nothing Nothing (Data.List.NonEmpty.fromList [Four,Three,One,Two,Two]))
    buildWalls (inviteLord (snd (buildCastle (buildHouse (buildHouse (City Nothing Nothing (Data.List.NonEmpty.fromList [One, Two, Two])) 3) 4))) Lord) `shouldBe` City {cityCastle = Just (Castle {castleWalls = Just Walls, castleLord = Just Lord}), cityEducation = Nothing, cityHouses = (Data.List.NonEmpty.fromList [Four,Three,One,Two,Two])}
    inviteLord (snd (buildCastle (buildHouse (buildHouse (City Nothing Nothing (Data.List.NonEmpty.fromList [One, Two, Two])) 3) 4))) Lord `shouldBe` City {cityCastle = Just (Castle {castleWalls = Nothing, castleLord = Just Lord}), cityEducation = Nothing, cityHouses = (Data.List.NonEmpty.fromList [Four,Three,One,Two,Two])}

  it "Tree" $ do
    find 5  (Hw1.fromList [3, 1, 4, 1, 4, 5])         `shouldBe` Just 5
    find 4  (Hw1.fromList [3, 1, 4, 1, 4, 5])         `shouldBe` Just 4
    (toList . Hw1.fromList) [3, 12, 39, 1]            `shouldBe` [1, 3, 12, 39]
    (toList . Hw1.fromList) [3, 12, 42, 1]            `shouldBe` [1, 3, 12, 42]
    (toList . insert 7 . Hw1.fromList) [3, 12, 42, 1] `shouldBe` [1, 3, 7, 12, 42]
    (toList . insert 1 . Hw1.fromList) [3, 12, 42, 1] `shouldBe` [1, 1, 3, 12, 42]
    size (Hw1.fromList [3, 1, 4, 1, 4, 5])            `shouldBe` 4
    isEmpty (remove 12 (Hw1.fromList [12]))           `shouldBe` True
    isEmpty (Hw1.fromList [1, 2])                     `shouldBe` False

