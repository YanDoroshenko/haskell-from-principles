module Excercises where

import Data.List (intersperse)

infixl 5 ***
(***) :: (Integral a) => a -> a -> a
x *** y =
  go 0 x y
    where
      go acc x 0 = acc
      go acc x y = go (acc + x) x (y - 1)

sum' :: (Eq a, Num a) => a -> a
sum' 1 = 1
sum' n = n + sum' (n - 1)

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy num 0 = Nothing
dividedBy num denom
  | num > 0 && denom > 0 = Just $ go num denom 0
  | num < 0 && denom < 0 = Just $ go (abs num) (abs denom) 0
  | num > 0 && denom < 0 = f <$> (Just $ go num (abs denom) 0)
  | num < 0 && denom > 0 = f <$> (Just $ go (abs num) denom 0)
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)
    f (x, y) = (-x, y)

mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 $ n + 11

digitToWord :: Int -> String
digitToWord n = case n of
                  0 -> "zero"
                  1 -> "one"
                  2 -> "two"
                  3 -> "three"
                  4 -> "four"
                  5 -> "five"
                  6 -> "six"
                  7 -> "seven"
                  8 -> "eight"
                  9 -> "nine"
                  _ -> ""

digits :: Int -> [Int]
digits 0 = []
digits n = (digits d) ++ [m]
  where
    (d, m) = divMod n 10

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ digitToWord <$> (digits n)

