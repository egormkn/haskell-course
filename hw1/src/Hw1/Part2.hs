module Hw1.Part2 where

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt n list@(x : xs)
  | n < 0     = (Nothing, list)
  | n == 0    = (Just x, xs)
  | otherwise = (fst pair, x : snd pair) where pair = removeAt (n - 1) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [x]  = [x]
mergeSort list =
  let (left, right) = split list
  in merge (mergeSort left) (mergeSort right)
  where
    split :: [a] -> ([a], [a])
    split []           = ([], [])
    split [x]          = ([x], [])
    split (x : y : xs) = let (evens, odds) = split xs in (x : evens, y : odds)

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] r  = r
    merge l  [] = l
    merge a@(x : xs) b@(y : ys)
      | x < y     = x : merge xs b
      | otherwise = y : merge a ys
