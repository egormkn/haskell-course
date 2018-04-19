{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Hw2.Part3 where

import Control.Applicative
import Control.Monad

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser $ fmap (first f) . parser

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser pf <*> Parser pa = Parser $ pf >=> (\ t -> (pa . snd) t >>= \(a, r) -> Just (fst t a, r))

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser a >>= f = undefined

instance Alternative (Parser s) where
  empty :: Parser s a  -- always fails
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a  -- run first, if fails — run second
  Parser a <|> Parser b = undefined

-------------------------------------------------

ok :: Parser s ()
ok  = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \case
          [] -> Just ((), [])
          _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs) | p x       = Just (x, xs)
             | otherwise = Nothing

element :: Eq s => s -> Parser s s
element c = satisfy (== c)

stream :: Eq s => [s] -> Parser s [s]
stream = traverse element

-------------------------------------

psp :: Parser String String
psp = undefined
{-
Задание 3: Простые парсеры

Используя существующие комбинаторы (реализовав по необходимости остальные) напишите следующие парсеры строковых потоков:

    Парсер правильных скобочных последовательностей (падает, если последовательность неправильная, и не падает, если правильная).
    Парсер целого числа, перед которым может быть знак + или -.

Задание 4: Непростой парсер

Написать парсер списка списков чисел, разделённых запятой. Все числа перечисленны через запятую. В начале каждого списка находится число — длина списка. Таким образом можно понять, где заканчивается каждый список. То есть список

[ [1, 10], [5, -7, 2] ]

в строковом виде может быть записан следующим образом:

"2, 1,+10  , 3,5,-7, 2"
-}
