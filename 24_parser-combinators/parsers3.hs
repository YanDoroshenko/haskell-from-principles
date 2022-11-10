module Parsers3 where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative
import Data.Maybe

data NumberOrString = NOSS String | NOSI Integer deriving Show
type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
data SemVer = SemVer Major Minor Patch Release Metadata deriving Show

v = "1.0.0-x.7.z.92"

parseSemVer :: Parser SemVer
parseSemVer = undefined

testParse p s = print $ parseString p mempty s

parseFirst = do
  major <- (integer :: Parser Integer)
  _ <- skipSome (char '.')
  minor <- (integer :: Parser Integer)
  _ <- skipSome (char '.')
  patch <- (integer :: Parser Integer)
  _ <- skipOptional (char '-')
  maybeRelease <- optional $ some $ (NOSI <$> try decimal) <|> (NOSS <$> (some (token (noneOf "+.")))) >>= (\x -> (const (x)) <$> (optional dot))
  let release = fromMaybe [] maybeRelease
  _ <- skipOptional (char '+')
  maybeMetadata <- optional $ some $ (NOSI <$> try decimal) <|> (NOSS <$> (some (token (notChar '.')))) >>= (\x -> (const (x)) <$> (optional dot))
  let metadata = fromMaybe [] maybeMetadata
  return $ SemVer major minor patch release metadata

instance Eq SemVer where
  (SemVer majorL minorL patchL _ _) == (SemVer majorR minorR patchR _ _) = majorL == majorR && minorL == minorR && patchL == patchR

instance Ord SemVer where
  (SemVer majorL minorL patchL _ _) <= (SemVer majorR minorR patchR _ _) = majorL <= majorR && minorL <= minorR && patchL <= patchR

parseDigit :: Parser Char
parseDigit = oneOf(['0', '1'..'9'])

base10Integer :: Parser Integer
base10Integer = do
  maybeSign <- optional $ char '-'
  let sign = fromMaybe "" $ (\x -> [x]) <$> maybeSign
  number <- some parseDigit
  return (read (sign ++ number) :: Integer)
