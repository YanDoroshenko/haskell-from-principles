module EnumFromTo where
import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True True = [True]
eftBool False False = [False]
eftBool _ _ = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT GT = [GT]
eftOrd _ _ = []

eftInt :: Int -> Int -> [Int]
eftInt l r
  | l == r = [l]
  | otherwise = l : (eftInt (l + 1) r)

eftChar :: Char -> Char -> [Char]
eftChar l r
  | l == r = [l]
  | otherwise = l : (eftChar (chr (ord l + 1)) r)
