{-# LANGUAGE InstanceSigs #-}

module Hw2.Part2 where

import Control.Monad
import Text.Read

----------------------------------------------------

stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse readMaybe . words

----------------------------------------------------

newtype Optional a = Optional (Maybe (Maybe a)) deriving Show

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap f (Optional opt) = Optional $ fmap (fmap f) opt

instance Applicative Optional where
  pure :: a -> Optional a
  pure = Optional . Just . Just

  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  Optional f <*> Optional opt = Optional $ liftM2 (<*>) f opt

instance Monad Optional where
  (>>=) :: Optional a -> (a -> Optional b) -> Optional b
  Optional Nothing         >>= _ = Optional Nothing
  Optional (Just Nothing)  >>= _ = Optional (Just Nothing)
  Optional (Just (Just a)) >>= f = f a

instance Foldable Optional where
  foldr :: (a -> b -> b) -> b -> Optional a -> b
  foldr _ z (Optional Nothing)         = z
  foldr _ z (Optional (Just Nothing))  = z
  foldr f z (Optional (Just (Just x))) = f x z

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ (Optional Nothing)         = pure (Optional Nothing)
  traverse _ (Optional (Just Nothing))  = pure (Optional (Just Nothing))
  traverse f (Optional (Just (Just x))) = Optional . Just . Just <$> f x

--------------------------------------------------

data NonEmpty a = a :| [a]

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (a :| as) = f a :| fmap f as

  (<$) :: b -> NonEmpty a -> NonEmpty b
  b <$ (_ :| as) = b :| (b <$ as)

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure a = a :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) = ap

  (*>) :: NonEmpty a -> NonEmpty b -> NonEmpty b
  (*>) (x :| xs) (y :| ys) = undefined

  (<*) :: NonEmpty a -> NonEmpty b -> NonEmpty a
  (<*) (x :| xs) (y :| ys) = undefined

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) a b = undefined
  (>>) :: NonEmpty a -> NonEmpty b -> NonEmpty b
  (>>) a b = undefined
  return :: a -> NonEmpty a
  return a = undefined

{-instance Monad NonEmpty where
  ~(a :| as) >>= f = b :| (bs ++ bs')
    where b :| bs = f a
          bs' = as >>= toList . f
          toList ~(c :| cs) = c : cs
-}
{-
instance Functor [] where
    fmap = map

-- See Note: [List comprehensions and inlining]
-- | @since 2.01
instance Applicative [] where
    pure x    = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
    xs *> ys  = [y | _ <- xs, y <- ys]

-- See Note: [List comprehensions and inlining]
-- | @since 2.01
instance Monad []  where
    xs >>= f             = [y | x <- xs, y <- f x]
    (>>) = (*>)
    fail _              = []
-}
