module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs@(Cons h t) = Cons (f h) (f <$> t) +++ (fs <*> xs)
    where
      Nil +++ r = r
      l +++ Nil = l
      (Cons l ls) +++ r = Cons l $ ls +++ r

instance Eq a => EqProp (List a) where
  Nil =-= Nil = True `eq` True
  (Cons h t) =-= (Cons h' t') = h `eq` h' .&&. (t =-= t')
  _ =-= _ = False `eq` True

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = f <$> (arbitrary `suchThat` (\x -> length x < 5))
    where
      f [] = Nil
      f (h : t) = Cons h (f t)

main = quickBatch $ applicative $ Cons ("a", "b", "c") $ Cons ("b", "c", "d") Nil
