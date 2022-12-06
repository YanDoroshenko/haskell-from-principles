module RollYourOwn where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use 'error'
    -- _extremely_ sparingly.
    x ->
      error $
      "intToDie got non 1-6 integer: "
      ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = f 0 0 [] g
  where f sum count log gen
          | sum >= n = (count, log)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in f (sum + die) (count + 1) (log ++ [intToDie die]) nextGen
