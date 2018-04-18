module Hw1.Part1 where

import           Data.List (sort)

-- Упорядочить по возрастанию переданную тройку элементов
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = let [a, b, c] = sort [x, y, z] in (a, b, c)

-- Повторить каждый элемент столько раз, чему равен сам элемент
smartReplicate :: [Int] -> [Int]
smartReplicate list = concat $ map (\x -> replicate x x) list

-- Вернуть список только тех списков, которые содержат переданный элемент
contains :: Eq a => a -> [[a]] -> [[a]]
contains x list = filter (elem x) list

-- Найти сумму всех чисел в строке
stringSum :: String -> Int
stringSum string = sum $ map read $ words string
