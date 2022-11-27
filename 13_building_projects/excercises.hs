module Excercises where

import Data.Char
import Data.List
import Control.Monad
import System.Exit (exitSuccess)

caesar :: IO ()
caesar = do
  str <- getLine
  n <- (read :: String -> Int) <$> getLine
  putStrLn $ f n str where
    f n s = case s of
                 "" -> ""
                 h : t -> chr (ord 'a' + ((ord h - ord 'a' + n) `mod` 26)) : (f n t)

vigenere :: IO ()
vigenere = do
  str <- getLine
  cipher <- getLine
  let l = length str
  let cipherStr = take l $ concat $ repeat cipher
  putStrLn $ f <$> (zip str cipherStr) where
      f (c, c') = unNum $ (num c) + (num c')
      num c = (ord c) - (ord 'a')
      unNum n = chr $ n `mod` 26 + (ord 'a')

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let normalizedLine = toLower <$> (filter isAlpha line1)
  case (normalizedLine == reverse normalizedLine) of
    True -> putStrLn "It's a palindrome"
    False -> putStrLn "Nope" >> exitSuccess

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =  Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++
    " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  age <- (read :: String -> Integer) <$> getLine
  case mkPerson name age of
    (Right p) -> putStrLn $ "Yay! Successfully got a person: " ++ (show p)
    (Left e) -> putStrLn "Error occured!" >> (putStrLn $ show e)
