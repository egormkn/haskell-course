module Hw1.Part3 where

import Data.List.NonEmpty (NonEmpty (..), toList, (<|))

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Show, Enum, Bounded)

-- Task 1: Days of week

nextDay :: Day -> Day
nextDay day
  | day == maxBound = minBound
  | otherwise       = succ day

afterDays :: Int -> Day -> Day
afterDays n day
  | n `mod` 7 == 0 = day
  | otherwise      = afterDays (n - 1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend day = day `elem` [Sat, Sun]

daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty day = 1 + daysToParty (nextDay day)

-- Task 2: Building castles

data City = City {
    cityFort    :: Maybe (Castle, Maybe Walls),
    cityCulture :: Maybe Culture,
    cityHouses  :: NonEmpty House
} deriving (Show, Eq)

data Castle = Castle {
    castleLord  :: Maybe Lord
} deriving (Show, Eq)

data Walls = Walls
  deriving (Show, Eq)

data Culture = Library | Church
  deriving (Show, Eq)

data House = One | Two | Three | Four
  deriving (Show, Enum, Eq)

data Lord = Lord
  deriving (Show, Eq)

buildCastle :: City -> (Bool, City)
buildCastle city@City { cityFort = Nothing } =
  let castle = Castle { castleLord = Nothing } in 
    (True, city { cityFort = Just (castle, Nothing) })
buildCastle city = (False, city)

buildCulture :: Culture -> City -> (Bool, City)
buildCulture culture city@City { cityCulture = Nothing } =
  (True, city { cityCulture = Just culture })
buildCulture _ city = (False, city)

buildChurch :: City -> (Bool, City)
buildChurch = buildCulture Church

buildLibrary :: City -> (Bool, City)
buildLibrary = buildCulture Library

buildHouse :: City -> Int -> (Bool, City)
buildHouse city@City { cityHouses = houses } members
  | members `elem` [1 .. 4] = (True, city { cityHouses = toEnum (members - 1) <| houses })
  | otherwise               = (False, city)

castleHasLord :: Castle -> Bool
castleHasLord Castle { castleLord = Just _ } = True
castleHasLord _                              = False

cityHasWalls :: City -> Bool
cityHasWalls City { cityFort = Just (_, Just _) } = True
cityHasWalls _                                    = False

inviteLord :: City -> Lord -> Either String City
inviteLord City { cityFort = Nothing } _ = Left "There is no castle in the city"
inviteLord city@City { cityFort = Just (castle, walls) } lord
  | castleHasLord castle = Left "Another lord is living in the castle"
  | otherwise = Right city { cityFort = Just (castle { castleLord = Just lord }, walls) }

cityPopulation :: City -> Int
cityPopulation City { cityHouses = houses } =
  sum $ map ((+ 1) . fromEnum) $ Data.List.NonEmpty.toList houses

buildWalls :: City -> Either String City
buildWalls city@City { cityFort = Just (castle, _) }
  | not (castleHasLord castle)  = Left "City doesn't have a lord"
  | cityPopulation city < 10    = Left "City population is too small"
  | cityHasWalls city           = Left "There are already walls in the city"
  | otherwise                   = Right city { cityFort = Just (castle, Just Walls) }
buildWalls City { cityFort = Nothing } = Left "No castle in the city"

-- Task 3: Natural numbers

data Nat = Z | S Nat deriving Show

instance Eq Nat where
  (==) Z     Z     = True
  (==) (S a) (S b) = a == b
  (==) _     _     = False

instance Num Nat where
  -- �������� ���� ����ࠫ��� �ᥫ
  (+) x Z     = x
  (+) Z y     = y
  (+) x (S y) = S (x + y)

-- ���⠭�� ����ࠫ��� �ᥫ
  (-) x     Z     = x
  (-) Z     _     = Z
  (-) (S x) (S y) = x - y

-- ��������� ���� ����ࠫ��� �ᥫ
  (*) _ Z     = Z
  (*) Z _     = Z
  (*) x (S y) = x + (x * y)

-- ����� ����ࠫ쭮�� �᫠
  abs = id

-- ���� ����ࠫ쭮�� �᫠
  signum Z = 0
  signum _ = 1

-- �ॢ�饭�� 楫�� �ᥫ � ����ࠫ��
  fromInteger 0 = Z
  fromInteger x | x < 0     = error "Not a natural number"
                | otherwise = S (fromInteger (x - 1))

instance Ord Nat where
  -- �ࠢ����� ����ࠫ��� �ᥫ
  (<=) (S _) Z     = False
  (<=) (S x) (S y) = x <= y
  (<=) _     _     = True

instance Integral Nat where
  -- �����᫥���� ������� ����ࠫ��� �ᥫ � ���⮪ �� ������� ����ࠫ쭮�� �᫠ �� ��㣮�
  quotRem _ Z = error "Division by zero"
  quotRem x y =
    let division (d, r) = if r >= y then division (S d, r - y) else (d, r)
    in  division (Z, x)

-- �ॢ�饭�� ����ࠫ��� �ᥫ � 楫�
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
find x (Node ns a b) | head ns == x = Just x
                     | head ns > x  = find x a
                     | otherwise    = find x b

insert :: (Ord a) => a -> Tree a -> Tree a
insert _ (Node [] _ _) = error "Insert failed"
insert x Leaf          = Node [x] Leaf Leaf
insert x (Node ns@(n : _) a b) | n == x    = Node (ns ++ [x]) a b
                               | x < n     = Node ns (insert x a) b
                               | otherwise = Node ns a (insert x b)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Leaf

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node x l r) = Hw1.Part3.toList l ++ x ++ Hw1.Part3.toList r

remove :: (Ord a) => a -> Tree a -> Tree a
remove _ (Node [] _ _) = error "Remove failed"
remove _ Leaf          = Leaf
remove x (Node ns@(n : _) a b)
  | x < n                  = Node ns (remove x a) b
  | x > n                  = Node ns a (remove x b)
  | isEmpty a && isEmpty b = Leaf
  | isEmpty b              = a
  | otherwise              = Node (findMin b) a (remove (head $ findMin b) b)

findMin :: (Ord a) => Tree a -> [a]
findMin Leaf                = error "FindMin failed"
findMin (Node ns Leaf Leaf) = ns
findMin (Node _  a    _   ) = findMin a
