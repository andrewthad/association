{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.Association
  ( someFunc
  , Association
  , singleton
  , empty
  , lookup
  , insertWith
  , unionWith
  , foldlWithKey'
  ) where

import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Association k v
  = Cons k v (Association k v)
  | Nil
  deriving (Show)

instance (Eq k, Eq v) => Eq (Association k v) where
  (==) = eqAssoc

instance (Eq k, Monoid v) => Monoid (Association k v) where
  mempty = empty
  mappend = unionWith mappend

empty :: Association k v
empty = Nil

singleton :: k -> v -> Association k v
singleton k v = Cons k v Nil

lookup :: Eq k => k -> Association k v -> Maybe v
lookup _ Nil =  Nothing
lookup key (Cons x y xys)
  | key == x = Just y
  | otherwise = lookup key xys

insertWith :: Eq k => (v -> v -> v) -> k -> v -> Association k v -> Association k v
insertWith _ k v Nil = singleton k v
insertWith f k v (Cons x y xys)
  | k == x = Cons x (f v y) xys
  | otherwise = Cons x y (insertWith f k v xys)

unionWith :: Eq k => (v -> v -> v) -> Association k v -> Association k v -> Association k v
unionWith f = foldlWithKey' (\xs k v -> insertWith f k v xs)

foldlWithKey' :: (a -> k -> v -> a) -> a -> Association k v -> a
foldlWithKey' _ a Nil = a
foldlWithKey' f !a (Cons k v xs) = foldlWithKey' f (f a k v) xs

eqAssoc :: (Eq k, Eq v) => Association k v -> Association k v -> Bool
eqAssoc as = foldlWithKey' (\b k v -> b && maybe False (== v) (lookup k as)) True

