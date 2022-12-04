module ConstantInstance where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  _ <*> (Constant a) = Constant a

instance Eq a => EqProp (Constant a b) where
  (Constant a) =-= (Constant a') = eq a a'

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

constantAp :: Constant [Bool] (Bool, Bool, Bool)
constantAp = pure (True, True, True)

main = quickBatch $ applicative constantAp
