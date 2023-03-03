module Exercises where

import System.IO
import System.Exit
import System.Environment
import Data.Char
import Control.Monad
import Control.Monad.State

vigenere :: (Int -> Int -> Int) -> String -> String -> String
vigenere g cipher str =
  f <$> (zip str cipherStr)
    where
      f (c, c') = unNum $ g (num c) (num c')
      num c = ord c - ord 'a' + 1
      unNum n = chr $ ord 'a' + ((n + 25) `mod` 26)
      cipherStr = take l $ concat $ repeat cipher
      l = length str

decode = vigenere (-)
encode = vigenere (+)

main = do
  args <- getArgs
  forever $ do
    let f =
          if (elem "-d" args) then decode
          else if (elem "-e" args) then encode
          else (\_ _ -> "no-op")
        key = head $ filter (\x -> not $ elem x ["-d", "-e"]) args
    inTime <- hWaitForInput stdin 5000
    if (inTime) then do
      string <- getLine
      print $ f key string
    else do
      hPutStr stderr "Timeout!\n"
      exitFailure
