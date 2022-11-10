{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative
import Data.Ratio ((%))

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse p s = print $ parseString p mempty s

one'' = one >> eof

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

parseFractionOrInt :: Parser (Either Rational Integer)
parseFractionOrInt = try (Left <$> parseFraction) <|> (Right <$> decimal)
