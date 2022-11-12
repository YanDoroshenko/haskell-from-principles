module ScansExcercises where

fibs = 1 : scanl (+) 1 fibs

fibs' = take 20 fibs

fibs'' = takeWhile (\x -> x < 100) (1 : scanl (+) 1 fibs'')

factorial = scanl (\l r -> l * r) 1 [1..]
