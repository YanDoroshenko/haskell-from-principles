module Exercises where

import Criterion.Main
import qualified Data.Sequence as S(Seq(..), drop, index, null, (<|), empty)

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ \_ -> []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton h = DL $ \_ -> [h]
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList l = unDL l []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixr `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL ((++ [x]) . unDL xs)
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL $ (unDL xs) . (unDL ys)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [
  bench "concat list" $ whnf schlemiel 123456,
  bench "concat dlist" $ whnf constructDlist 123456
  ]

main' :: IO ()
main' = defaultMain [
  bench "queue list" $ whnf queue 1000,
  bench "queue sequence" $ whnf queue' 1000
                    ]

queue :: Int -> Queue Int
queue x = foldr (\l r ->
  case pop $ push l r of
    Just (_, q') -> q'
    Nothing -> r
                 ) (Queue [] []) [1..x]

queue' :: Int -> Queue' Int
queue' x = foldr (\l r ->
  case pop' $ push' l r of
    Just (_, q') -> q'
    Nothing -> r
                 ) (Queue' S.empty) [1..x]

data Queue a =
  Queue { enqueue :: [a]
  , dequeue :: [a]
  } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue e d) = Queue (a : e) d

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue e []) = Just (last e, Queue [] (reverse $ init e))
pop (Queue e d) = Just (head d, Queue e (tail d))

data Queue' a = Queue' (S.Seq a) deriving (Eq, Show)

push' :: a -> Queue' a -> Queue' a
push' a (Queue' q) = Queue' (a S.<| q)

pop' :: Queue' a -> Maybe (a, Queue' a)
pop' (Queue' q)
  | S.null q = Nothing
  | otherwise = Just (S.index q 1, Queue' $ S.drop 1 q)

