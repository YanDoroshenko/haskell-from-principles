module Vector where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Criterion.Main

unboxed :: Int -> U.Vector Int
unboxed = U.iterateN 1 (+1)

boxed :: Int -> V.Vector Int
boxed = V.iterateN 1 (+1)

main = defaultMain [
  bench "unboxed vector" $ whnf unboxed 9999,
  bench "boxed vector" $ whnf boxed 9999
  ]
