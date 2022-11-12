module ThyFearfulSymmetry where

myWords :: String -> [String]
myWords "" = []
myWords str =
  (takeWhile isNotSpace str) : (myWords $ dropWhile (not . isNotSpace) $ dropWhile isNotSpace str) where
    isNotSpace = \x -> x /= ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
  \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines str = (takeWhile isNotNewLine str) : (myLines $ dropWhile (not . isNotNewLine) $ dropWhile isNotNewLine str) where
    isNotNewLine = \x -> x /= '\n'

mySeparator :: Eq a => a -> [a] -> [[a]]
mySeparator _ [] = []
mySeparator separator arr = (takeWhile isNotSeparator arr) : (mySeparator separator $ dropWhile (not . isNotSeparator) $ dropWhile isNotSeparator arr) where
    isNotSeparator = \x -> x /= separator
