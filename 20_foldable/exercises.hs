module Exercises where

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' a b b') = f b' z

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' a b b' b'') = f b'' z

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap g
  where
    g x = if (f x) then pure x else mempty
