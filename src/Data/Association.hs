{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.Association
  ( Association
  , singleton
  , empty
  , null
  , lookup
  , insertWith
  , unionWith
  , foldlWithKey'
  , valid
  ) where

import Prelude hiding (lookup,null)
import Data.Functor.Classes
import qualified GHC.OldList as L

data Association k v
  = Cons k v (Association k v)
  | Nil
  deriving (Show,Functor,Foldable,Traversable)

instance (Eq k, Eq v) => Eq (Association k v) where
  (==) = eqAssoc

instance Eq k => Eq1 (Association k) where
  liftEq = liftEqAssoc2 (==)

instance Eq2 Association where
  liftEq2 = liftEqAssoc2

instance (Eq k, Monoid v) => Monoid (Association k v) where
  mempty = empty
  mappend = unionWith mappend

empty :: Association k v
empty = Nil

null :: Association k v -> Bool
null Nil = True
null (Cons _ _ _) = False

singleton :: k -> v -> Association k v
singleton k v = Cons k v Nil

lookup :: Eq k => k -> Association k v -> Maybe v
lookup _ Nil =  Nothing
lookup key (Cons x y xys)
  | key == x = Just y
  | otherwise = lookup key xys

liftLookup :: (k1 -> k2 -> Bool) -> k1 -> Association k2 v -> Maybe v
liftLookup _ _ Nil =  Nothing
liftLookup eqK key (Cons x y xys)
  | eqK key x = Just y
  | otherwise = liftLookup eqK key xys

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

-- This should be rewritten to avoid the flipping of eqK.
liftEqAssoc2 :: (k1 -> k2 -> Bool) -> (v1 -> v2 -> Bool) -> Association k1 v1 -> Association k2 v2 -> Bool
liftEqAssoc2 eqK eqV as = foldlWithKey' (\b k v -> b && maybe False (`eqV` v) (liftLookup (flip eqK) k as)) True

-- | Test if the internal structure is valid. This checks to see
--   if there are any duplicate keys.
valid :: Eq k => Association k v -> Bool
valid = go [] where
  go :: Eq k => [k] -> Association k v -> Bool
  go _ Nil = True
  go ks (Cons k _ xs) = L.notElem k ks && go (k : ks) xs


