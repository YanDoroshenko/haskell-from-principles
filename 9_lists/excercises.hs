module Excercises where

import Data.Char

toUpper' :: String -> String
toUpper' x = case x of
               "" -> ""
               (h : t) -> (toUpper h) : t

toUpper'' :: String -> String
toUpper'' x = case x of
               "" -> ""
               (h : t) -> (toUpper h) : (toUpper'' t)

upperHead :: String -> Char
upperHead = toUpper . head

caesar :: Int -> String -> String
caesar n str = case str of
                 "" -> ""
                 h : t -> chr (ord 'a' + ((ord h - ord 'a' + n) `mod` 26)) : (caesar n t)

uncaesar n = caesar $ -n

myOr :: [Bool] -> Bool
myOr [] = True
myOr (h : t) = h || myOr t

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (h : t) = (f h) || myAny f t

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (h : t) = h == a || myElem a t

myElem' :: Eq a => a -> [a] -> Bool
myElem' a as = myAny (\x -> x == a) as

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h : t) = myReverse t ++ [h]

squish :: [[a]] -> [a]
squish [] = []
squish (h : t) = h ++ squish t

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (h : t) = (f h) ++ squishMap f t

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (h : []) = h
myMaximumBy f (h : t) = case f ft h of
                          GT -> ft
                          _ -> h
                          where
                            ft = myMaximumBy f t

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (h : []) = h
myMinimumBy f (h : t) = case f ft h of
                          LT -> ft
                          _ -> h
                          where
                            ft = myMinimumBy f t

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMaximumBy compare
