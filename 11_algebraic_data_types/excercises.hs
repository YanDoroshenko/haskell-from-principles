{-# LANGUAGE LambdaCase #-}
module Excercises where

import Data.Char
import Data.List

vigenere :: String -> String -> String
vigenere cipher str =
  f <$> (zip str cipherStr)
    where
      f (c, c') = unNum $ (num c) + (num c')
      num c = (ord c) - (ord 'a')
      unNum n = chr $ n `mod` 26 + (ord 'a')
      cipherStr = take l $ concat $ repeat cipher
      l = length str

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(h1 : t1) (h2 : t2)
  | h1 == h2 = isSubseqOf t1 t2
  | otherwise = isSubseqOf xs t2

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = f <$> (words str)
  where
    f s = case s of
            s'@(h : t) -> (s', toUpper h : t)
            "" -> ("", "")

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (h : t) = toUpper h : t

capitalizeParagraph :: String -> String
capitalizeParagraph = foldl f "" . words
  where f l r
          | l == "" = capitalizeWord r
          | isSuffixOf "." l = l ++ " " ++ capitalizeWord r
          | otherwise = l ++ " " ++ r

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add l r) = (eval l) + (eval r)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add l r) = (printExpr l) ++ " + " ++ (printExpr r)

