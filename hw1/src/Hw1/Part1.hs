module Hw1.Part1 where

import Data.List (sort)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = let [a, b, c] = sort [x, y, z] in (a, b, c)

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem

stringSum :: String -> Int
stringSum = sum . map read . words
