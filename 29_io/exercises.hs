{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Exercises where

import Data.Char (ord, chr)
import Control.Monad
import System.IO
import System.Environment
import System.Exit
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Trifecta
import System.Directory

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


newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany $ do
    _ <- char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

data Section = Section Header Assignments deriving (Eq, Show)
newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

parseAllInis :: IO (Map String Config)
parseAllInis = do
  contents <- getDirectoryContents "."
  let inis = filter (isSuffixOf ".ini") contents
      readF f = (f,) <$> (withFile f ReadMode hGetContents')
      parseF = parseString parseIni mempty
  iniContents <- sequence $ readF <$> inis
  let res = (parseString parseIni mempty) `M.map` (M.fromList iniContents)
  return $ (foldResult (const $ Config M.empty) id) `M.map` res

