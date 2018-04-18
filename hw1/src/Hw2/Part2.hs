{-# LANGUAGE InstanceSigs #-}

module Hw2.Part2 where

import Text.Read

stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse readMaybe . words


newtype Optional a = Optional (Maybe (Maybe a))

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap f (Optional opt) = Optional $ opt >>= \x -> x >>= \v -> Just $ Just $ f v

instance Applicative Optional where
  pure :: a -> Optional a
  pure = Optional . Just . Just

  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  Optional Nothing <*> _ = Optional Nothing
  _ <*> Optional Nothing = Optional Nothing
  Optional f <*> v = fmap (f >>= \f1 -> f1 >>= \f2 -> f2) v
  -- Mb x@(Mb a->b) -> (x@(Mb a -> b) -> Mb )
  -- m x -> (x -> m y) -> m y
