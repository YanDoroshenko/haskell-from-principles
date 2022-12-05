module Exercises where

import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  _ =-= _ = True `eq` True

data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (Left' f) <*> (Left' a) = Left' $ f a
  (Right' f) <*> _ = Right' f
  _ <*> (Right' b) = Right' b

instance Monad (PhhhbbtttEither b) where
  (Left' a) >>= f = f a
  (Right' b) >>= _ = Right' b

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (Left' a) =-= (Left' a') = a `eq` a'
  (Right' b) =-= (Right' b') = b `eq` b'
  _ =-= _ = False `eq` True

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  (Identity a) >>= f = f a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (f <$> t)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> as@(Cons h t) = Cons (f h) $ (f <$> t) ++ (fs <*> as)
    where
      Nil ++ r = r
      l ++ Nil = l
      (Cons h t) ++ r = Cons h $ t ++ r

instance Monad List where
  Nil >>= _ = Nil
  (Cons h t) >>= f = f h ++ (t >>= f)
    where
      Nil ++ r = r
      l ++ Nil = l
      (Cons h t) ++ r = Cons h $ t ++ r

instance Eq a => EqProp (List a) where
  Nil =-= Nil = True `eq` True
  (Cons h t) =-= (Cons h' t') = h `eq` h' .&&. t =-= t'
  _ =-= _ = False `eq` True

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = f <$> arbitrary `suchThat` (\x -> length x < 5)
    where
      f [] = Nil
      f (h : t) = Cons h $ f t

j :: Monad m => m (m a) -> m a
j = (=<<) id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (h : t) f = f h >>= (\h' -> (\t' -> h' : t') <$> (meh t f))

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id

main = do
  quickBatch $ monad $ (undefined :: Nope (Int, Int, Int))
  quickBatch $ monad $ (undefined :: PhhhbbtttEither (Int, Int, Int) (Int, Int, Int))
  quickBatch $ monad $ (undefined :: Identity (Int, Int, Int))
  quickBatch $ monad $ (undefined :: List (Int, Int, Int))
