module Hw1.Part3 where

import Data.List.NonEmpty (NonEmpty(..), toList, (<|))

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Show, Enum, Bounded)

nextDay :: Day -> Day
nextDay day | day == maxBound = minBound
            | otherwise = succ day


afterDays :: Int -> Day -> Day
afterDays n day | n `mod` 7 == 0 = day
                | otherwise = afterDays (n - 1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend day = elem day [Sat, Sun]


daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty day = 1 + (daysToParty $ nextDay day)




data City = City {
    cityCastle    :: Maybe Castle,
    cityEducation :: Maybe Education,
    cityHouses    :: NonEmpty House
} deriving (Show, Eq)

data Castle = Castle {
    castleWalls :: Maybe Walls,
    castleLord  :: Maybe Lord
} deriving (Show, Eq)

data Education = Church | Library
            deriving (Show, Eq)

data House = One | Two | Three | Four
             deriving (Show, Enum, Eq)

data Walls = Walls
             deriving (Show, Eq)

data Lord = Lord
            deriving (Show, Eq)

buildCastle :: City -> (Bool, City)
buildCastle city@City {cityCastle = Nothing} = (True, city{cityCastle = castle})
                                                where castle = Just $ Castle Nothing Nothing
buildCastle city = (False, city)


buildEducation :: City -> Education -> (Bool, City)
buildEducation city@City {cityEducation = Nothing} education = (True, city {cityEducation = Just education})
buildEducation city _ = (False, city)


buildChurch :: City -> (Bool, City)
buildChurch city = buildEducation city Church


buildLibrary :: City -> (Bool, City)
buildLibrary city = buildEducation city Library


buildHouse :: City -> Int -> City
buildHouse city@City {cityHouses = hs} members
           | elem members [1 .. 4] = city {cityHouses = (toEnum (members - 1)) <| hs}
           | otherwise = city

castleHasLord :: Castle -> Bool
castleHasLord Castle {castleLord = Nothing} = False
castleHasLord _                             = True

castleHasWalls :: Castle -> Bool
castleHasWalls Castle {castleWalls = Nothing} = False
castleHasWalls _                              = True

inviteLord :: City -> Lord -> City
inviteLord city@City {cityCastle = (Just castle)} lord
           | castleHasLord castle = error "Another lord is living in the castle"
           | otherwise      = city {cityCastle = Just (castle {castleLord = Just lord})}
inviteLord City {cityCastle = Nothing} _ = error "No castle for lord in city"

cityPopulation :: City -> Int
cityPopulation City {cityHouses = houses} = sum $ map ((+1) . fromEnum) $ Data.List.NonEmpty.toList houses

buildWalls :: City -> City
buildWalls city@City {cityCastle = (Just castle)}
           | not (castleHasLord castle)   = error "City doesn't have a lord"
           | (cityPopulation city) < 10   = error "City is too small"
           | castleHasWalls castle        = error "There are walls in the city already"
           | otherwise = city {cityCastle = Just (castle {castleWalls = Just Walls})}
buildWalls City {cityCastle = Nothing} = error "No castle in the city"







-- Natural numbers
data Nat = Z | S Nat deriving Show

instance Eq Nat where
  -- Проверка натуральных чисел на равенство
  (==) Z Z         = True
  (==) (S a) (S b) = a == b
  (==) _ _         = False

instance Num Nat where
  -- Сложение двух натуральных чисел
  (+) x Z     = x
  (+) Z y     = y
  (+) x (S y) = S (x + y)

  -- Вычитание натуральных чисел
  (-) x Z         = x
  (-) Z _         = Z
  (-) (S x) (S y) = x - y

  -- Умножение двух натуральных чисел
  (*) _ Z     = Z
  (*) Z _     = Z
  (*) x (S y) = x + (x * y)

  -- Модуль натурального числа
  abs = id

  -- Знак натурального числа
  signum Z = 0
  signum _ = 1

  -- Превращение целых чисел в натуральные
  fromInteger 0 = Z
  fromInteger x | x < 0     = error "Not a natural number"
                | otherwise = S (fromInteger (x - 1))

instance Ord Nat where
  -- Сравнение натуральных чисел
  (<=) (S _) Z     = False
  (<=) (S x) (S y) = x <= y
  (<=) _ _         = True

instance Integral Nat where
  -- Целочисленное деление натуральных чисел и остаток от деления натурального числа на другое
  quotRem _ Z = error "Division by zero"
  quotRem x y = let division (d, r) = if r >= y then division (S d, r - y) else (d, r) in division (Z, x)

  -- Превращение натуральных чисел в целые
  toInteger Z     = 0
  toInteger (S x) = 1 + toInteger x

instance Real Nat where
  toRational x = toRational (toInteger x)

instance Enum Nat where
  toEnum x = fromInteger (fromIntegral x)
  fromEnum x = fromIntegral (toInteger x)



data Tree a = Leaf | Node [a] (Tree a) (Tree a)
  deriving (Show)

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf         = 0
size (Node _ a b) = 1 + size a + size b

find :: (Ord a) => a -> Tree a -> Maybe a
find _ Leaf = Nothing
find x (Node ns a b)
   | ns !! 0 == x  = Just x
   | ns !! 0 > x   = find x a
   | otherwise     = find x b

insert :: (Ord a) => a -> Tree a -> Tree a
insert _ (Node [] _ _) = error "Insert failed"
insert x Leaf = Node [x] Leaf Leaf
insert x (Node ns@(n:_) a b)
   | n == x    = Node (ns ++ [x]) a b
   | x < n     = Node ns (insert x a) b
   | otherwise = Node ns a (insert x b)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Leaf

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node x l r) = Hw1.Part3.toList l ++ x ++ Hw1.Part3.toList r

remove :: (Ord a) => a -> Tree a -> Tree a
remove _ (Node [] _ _) = error "Remove failed"
remove _ Leaf = Leaf
remove x (Node ns@(n:_) a b)
    | x < n = Node ns (remove x a) b
    | x > n = Node ns a (remove x b)
    | (isEmpty a) && (isEmpty b) = Leaf
    | isEmpty b = a
    | otherwise = Node (findMin b) a (remove (findMin b !! 0) b)

findMin :: (Ord a) => Tree a -> [a]
findMin Leaf                = error "FindMin failed"
findMin (Node ns Leaf Leaf) = ns
findMin (Node _ a _)        = findMin a
