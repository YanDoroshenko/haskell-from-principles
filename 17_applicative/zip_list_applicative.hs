module ZipListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons h t) = Cons h $ take' (n - 1) t

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs@(Cons h t) = Cons (f h) (f <$> t) +++ (fs <*> xs)
    where
      l +++ Nil = l
      Nil +++ r = r
      (Cons l ls) +++ r = Cons l $ ls +++ r

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs
        in take' 3000 l
      ys' = let (ZipList' l) = ys
        in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ pure a
  (ZipList' fs) <*> (ZipList' xs) = ZipList' $ fs <*> xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> f <$> (arbitrary `suchThat` (\x -> length x < 5))
    where
      f [] = Nil
      f (h : t) = Cons h (f t)

main = quickBatch $ applicative $ ZipList' $ Cons ("a", "b", "c") $ Cons ("b", "c", "d") Nil
