module Excercises where

tensDigit' :: Integral a => a -> a
tensDigit' = nthDigit 10

hunsD :: Integral a => a -> a
hunsD = nthDigit 100

nthDigit :: Integral a => a -> a -> a
nthDigit n = ((flip mod) 10) . fst . ((flip divMod) n)



foldBool :: a -> a -> Bool -> a
foldBool l _ True = l
foldBool _ r False = r

foldBool' :: a -> a -> Bool -> a
foldBool' l r b
  | b = l
  | otherwise = r

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show
