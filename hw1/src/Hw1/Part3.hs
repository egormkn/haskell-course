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

-- Natural numbers
data Nat = Z | S Nat

instance Eq Nat where
  -- Проверка натуральных чисел на равенство
  (==) Z Z = True
  (==) (S a) (S b) = a == b
  (==) _ _ = False

instance Num Nat where
  -- Сложение двух натуральных чисел
  (+) x Z = x
  (+) Z y = y
  (+) x (S y) = S (x + y)

  -- Вычитание натуральных чисел
  (-) x Z = x
  (-) Z y = Z
  (-) (S x) (S y) = x - y

  -- Умножение двух натуральных чисел
  (*) x Z = Z
  (*) Z y = Z
  (*) x (S y) = x + (x * y)

  -- Модуль натурального числа
  abs = id
  
  -- Знак натурального числа
  signum Z = 0
  signum _ = 1

  -- Превращение целых чисел в натуральные
  fromInteger 0 = Z
  fromInteger x = S (fromInteger (x - 1))

instance Ord Nat where
  -- Сравнение натуральных чисел
  (<=) (S x) Z = False
  (<=) (S x) (S y) = x <= y
  (<=) _ _ = True

instance Integral Nat where
  -- Целочисленное деление натуральных чисел и остаток от деления натурального числа на другое
  quotRem _ Z = error "Division by zero"
  quotRem x y = let division (div, rem) = if rem >= y then division (S div, rem - y) else (div, rem) in division (Z, x)

  -- Превращение натуральных чисел в целые
  toInteger Z = 0
  toInteger (S x) = 1 + toInteger x