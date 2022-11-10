module Eq where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn l) == (TisAn r) = l == r

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (Two l1 l2) == (Two r1 r2) = l1 == l2 && r1 == r2

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (TisAnInt l) == (TisAnInt r) = l == r
  (TisAString l) == (TisAString r) = l == r
  _ == _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (Pair l1 l2) == (Pair r1 r2) = l1 == l2 && r1 == r2

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a1 b1) == (Tuple a2 b2) = a1 == a2 && b1 == b2

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne a1) == (ThisOne a2) = a1 == a2
  (ThatOne a1) == (ThatOne a2) = a1 == a2
  _ == _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello a1) == (Hello a2) = a1 == a2
  (Goodbye b1) == (Goodbye b2) = b1 == b2
  _ == _ = False
