module Hw1.Part1 where

import           Data.List (sort)

-- ����冷��� �� �����⠭�� ��।����� �ன�� ����⮢
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = let [a, b, c] = sort [x, y, z] in (a, b, c)

-- ������� ����� ����� �⮫쪮 ࠧ, 祬� ࠢ�� ᠬ �����
smartReplicate :: [Int] -> [Int]
smartReplicate list = concat $ map (\x -> replicate x x) list

-- ������ ᯨ᮪ ⮫쪮 �� ᯨ᪮�, ����� ᮤ�ঠ� ��।���� �����
contains :: Eq a => a -> [[a]] -> [[a]]
contains x list = filter (elem x) list

-- ���� �㬬� ��� �ᥫ � ��ப�
stringSum :: String -> Int
stringSum string = sum $ map read $ words string
