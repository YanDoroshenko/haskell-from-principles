module Excercises where

import GHC.Float

stops = "pbtdkg"
vowels = "aeiou"

svs = [ (s, v, s) | s <- stops, v <- vowels ]

svs' = [ (s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p' ]

avgWordLength :: String -> Double
avgWordLength x = int2Double (sum (length <$> (words) x)) / int2Double(length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\l r -> f l || r) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\l r -> l == e || r) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (e ==)

myReverse :: [a] -> [a]
myReverse = foldr (\l r -> r ++ [l]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\l r -> f l : r) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\l r -> if (f l) then l : r else r) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\l r -> (f l) ++ r) []

squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f list = foldr (\l r -> case f l r of
                                 LT -> r
                                 _ -> l
                           ) (head list) list

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f list = foldr (\l r -> case f l r of
                                 GT -> r
                                 _ -> l
                           ) (head list) list
