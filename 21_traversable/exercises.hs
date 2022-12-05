{-# LANGUAGE FlexibleContexts #-}
module Exercises where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ z _ = z

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (f <$> t)

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons h t) = foldr f (f h z) t

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons h t) = Cons <$> (f h) <*> (traverse f t)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = f <$> arbitrary `suchThat` (\x -> length x < 3)
    where
      f [] = Nil
      f (h : t) = Cons h (f t)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> (f c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = (Pair a) <$> (f b)

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldr f z (Big _ b b') = f b $ f b' z

instance Traversable (Big a) where
  traverse f (Big a b b') = (Big a) <$> (f b) <*> (f b')

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldr f z (Bigger _ b b' b'') = f b $ f b' $ f b'' z

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = (Bigger a) <$> (f b) <*> (f b') <*> (f b'')

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Foldable n => Foldable (S n) where
  foldr f z (S na a) = f a $ foldr f z na

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> (traverse f na) <*> (f a)

data Tree a =
  Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

instance Foldable Tree where
  foldr f z (Leaf a) = f a z
  foldr f z (Node l a r) = f a z''
    where
      z'' = foldr f z' l
      z' = foldr f z r

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node l a r) = Node <$> (traverse f l) <*> (f a) <*> (traverse f r)

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = Node <$> (Leaf <$> arbitrary) <*> arbitrary <*> (Leaf <$> arbitrary)

main = do
  quickBatch $ traversable (undefined :: Identity (Maybe Any, Maybe Any, Any, Any))
  quickBatch $ traversable (undefined :: Constant (Maybe Any, Maybe Any, Any, Any) (Maybe Any, Maybe Any, Any, Any))
  quickBatch $ traversable (undefined :: Optional (Maybe Any, Maybe Any, Any, Any))
--  quickBatch $ traversable (undefined :: List (Maybe Any, Maybe Any, Any, Any))
  quickBatch $ traversable (undefined :: Three (Maybe Any, Maybe Any, Any, Any) (Maybe Any, Maybe Any, Any, Any) (Maybe Any, Maybe Any, Any, Any))
  quickBatch $ traversable (undefined :: Pair (Maybe Any, Maybe Any, Any, Any) (Maybe Any, Maybe Any, Any, Any))
  quickBatch $ traversable (undefined :: Big (Maybe Any, Maybe Any, Any, Any) (Maybe Any, Maybe Any, Any, Any))
  quickBatch $ traversable (undefined :: Bigger (Maybe Any, Maybe Any, Any, Any) (Maybe Any, Maybe Any, Any, Any))
  quickBatch $ traversable (undefined :: S Maybe (Maybe Any, Maybe Any, Any, Any))
  quickBatch $ traversable (undefined :: Tree (Maybe Any, Maybe Any, Any, Any))
