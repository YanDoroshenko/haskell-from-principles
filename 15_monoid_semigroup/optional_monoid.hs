module OptionalMonoid where

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

-- Since Haskell 4.11, Semigroup is a superclass of Monoid and Monoid instance can't exist without an instance of Semigroup
instance Semigroup a => Semigroup (Optional a) where
  Nada <> r = r
  l <> Nada = l
  (Only l) <> (Only r) = Only $ l <> r

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  First' Nada <> r = r
  l <> _ = l

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a
  -> First' a
  -> First' a
firstMappend = mappend
