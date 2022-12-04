module VariationsOnEither where

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure e) <*> (Failure e') = Failure $ e <> e'
  _ <*> (Failure e) = Failure e
  (Failure e) <*> _ = Failure e
  (Success f) <*> (Success a) = Success $ f a

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (Failure _) =-= (Success _) = False `eq` True
  (Success _) =-= (Failure _) = False `eq` True
  (Success a) =-= (Success a') = a `eq` a'
  (Failure e) =-= (Failure e') = e `eq` e'

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

validationAp :: Validation (String, String, String) (String, String, String)
validationAp = undefined

main = quickBatch $ applicative $ validationAp

