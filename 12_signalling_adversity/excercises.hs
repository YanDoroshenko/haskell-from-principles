module Excercises where

import Data.List

replaceThe :: String -> String
replaceThe = go ""
  where
    go "the " s = "a " ++ (go "" s)
    go "the" "" = "a"
    go acc "" = acc
    go acc s@(h : t) = if (acc `isPrefixOf` "the ") then go (acc ++ [h]) t else acc ++ go "" s

replaceThe' :: String -> String
replaceThe' = unwords . go . words
  where
    go [] = []
    go ("the" : t) = "a" : (go t)
    go (h : t) = h : (go t)

vowels = "aeiou"
countTheVowelInitial :: String -> Integer
countTheVowelInitial = go ""
  where
    go "the " (h : t) = if (h `elem` vowels) then 1 + go "" t else go "" t
    go acc "" = 0
    go acc s@(h : t) = if (acc `isPrefixOf` "the ") then go (acc ++ [h]) t else go "" s

countTheVowelInitial' :: String -> Integer
countTheVowelInitial' = go . words
  where
    go [] = 0
    go ("the" : s@(h : t) : t') = if (h `elem` vowels) then 1 + go (s : t') else go (s : t')

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (flip elem vowels)

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str
  | countVowels str > (fromIntegral (length str) - (countVowels str)) = Nothing
  | otherwise = Just $ Word' str

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n == 0 = Just Zero
  | n < 0 = Nothing
  | otherwise = Succ <$> (integerToNat (n - 1))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe z _ Nothing = z
mayybe _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing = z
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (h : _) = Just h

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : t) = catMaybes t
catMaybes ((Just h) : t) = h : (catMaybes t)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : _) = Nothing
flipMaybe ((Just h) : t) = (h :) <$> (flipMaybe t)


lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left l) r = l : r
    f (Right l) r = r

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Left l) r = r
    f (Right l) r = l : r

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where
    f (Left l) (ls, rs) = (l : ls, rs)
    f (Right r) (ls, rs) = (ls, r : rs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : (myIterate f $ f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = case f z of
                  Just (a, z') -> a : (myUnfoldr f z')
                  Nothing -> []

myIterate' f = myUnfoldr (\a -> Just (a, f a))

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f a = case f a of
               Nothing -> Leaf
               Just (a1, b, a2) -> Node (unfoldTree f a1) b (unfoldTree f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree (\a -> if (a < n) then Just (a + 1, a, a + 1) else Nothing) 0
