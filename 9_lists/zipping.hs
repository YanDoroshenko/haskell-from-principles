module Zipping where

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a : as) (b : bs) = (a, b) : (zip' as bs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (a : as) (b : bs) = (f a b) : zipWith' f as bs

zip'' = zipWith' (,)
