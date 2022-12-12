module Exercises where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Char (toLower)
import Data.List (intersperse, unfoldr)
import Data.Map (Map, alter, empty, singleton)
import Data.Maybe
import Data.Word
import Text.Trifecta
import Text.Parser.Combinators

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
  number <- some parseDigit
  return (read number :: Integer)

base10Integer' :: Parser Integer
base10Integer' = do
  maybeSign <- optional $ char '-'
  let applySign = fromMaybe id $ (const ((-1)*)) <$> maybeSign
  int <- base10Integer
  return $ applySign int

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- skipOptional $ string "+1"
  _ <- skipOptional $ string "1-"
  _ <- skipOptional $ char '('
  numberingPlanArea <- count 3 $ digit
  _ <- skipOptional $ char ')'
  _ <- skipOptional $ char ' '
  _ <- skipOptional $ char '-'
  exchange <- count 3 $ digit
  _ <- skipOptional $ char '-'
  lineNumber <- count 4 $ digit
  return $ PhoneNumber (read numberingPlanArea) (read exchange) (read lineNumber)

parseLog :: Parser [(String, [(Int, Int, String)])]
parseLog = do
  activities <- some day
  return activities
  where
    comment = try $ do
      _ <- skipOptional space
      _ <- string "--"
      _ <- skipOptional space
      _ <- rest
      return ()
    day = do
      _ <- skipOptional comment
      _ <- skipOptional newline
      _ <- char '#'
      _ <- space
      y <- count 4 digit
      _ <- char '-'
      m <- count 2 digit
      _ <- char '-'
      d <- count 2 digit
      _ <- skipOptional newline
      _ <- skipOptional comment
      _ <- skipOptional newline
      activities <- some $ do
        h <- count 2 digit
        _ <- char ':'
        m <- count 2 digit
        _ <- char ' '
        activity <- rest
        _ <- skipOptional comment
        return (read h, read m, activity)
      _ <- skipOptional newline
      _ <- skipOptional comment
      _ <- skipOptional newline
      return (mconcat $ intersperse "-" [y, m, d], activities)
    rest = manyTill anyChar $ (const () <$> comment) <|> eof <|> (const () <$> newline)

sumActivities :: Parser (Map String Int)
sumActivities = (snd . aggregate . activities) <$> parseLog
  where
    aggregate = foldr f (Nothing, empty)
    activities = (=<<) snd
    (h, m) /-/ (h', m')
      | h < h' = (24 - h') * 60 + 0 - m'
      | otherwise = (24 - h') * 60 + 0 - m'
    f (h, m, a) (Nothing, empty) = (Just (h, m), singleton a $ (24, 0) /-/ (h, m))
    f (h, m, a) (Just t, as) = (Just (h, m), newmap t as h m a)
    newmap t as h m a = alter (\a' -> Just $ (fromMaybe 0 a') + (t /-/ (h, m))) a as

data IPAddress = IPAddress Word32 deriving (Eq, Ord)

parseIpV4 :: Parser IPAddress
parseIpV4 = do
  w1 <- upto 3 digit
  _ <- char '.'
  w2 <- upto 3 digit
  _ <- char '.'
  w3 <- upto 3 digit
  _ <- char '.'
  w4 <- upto 3 digit
  _ <- eof
  return $ IPAddress (sum $ zipWith (\i x -> x * 256 ^ i) [3,2..] (read <$> [w1, w2, w3, w4]) :: Word32)
    where
      upto n p | n > 0 = (:) <$> (try p) <*> (upto (n - 1) p) <|> return []
      upto _ _ = return []

instance Show IPAddress where
  show (IPAddress w) = mconcat $ intersperse "." $ show <$> unfoldr unfold (3, w)
    where
      unfold (_, 0) = Nothing
      unfold (i, r) =
        let (d, m) = r `divMod` (256 ^ i)
         in Just (d, (i - 1, m))

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

parseIpV6 :: Parser IPAddress6
parseIpV6 = do
  ws <- upto 8 $ do
    w <- block
    _ <- eof <|> (fmap (const ()) $ char ':')
    return w
  let ws1 = take 4 ws
      ws2 = drop 4 ws
  _ <- eof
  return $ IPAddress6 (m ws1) (m ws2)
    where
      block = upto 4 hexDigit
      upto n p | n > 0 = (:) <$> (try p) <*> (upto (n - 1) p) <|> return []
      upto _ _ = return []
      compressedL ws = (length ws) - (length $ filter (not . null) ws)
      uncompressed ws = (\(_, s, _) -> s) $ foldl f (False, [], compressedL ws) ws
      f (False, acc, l) "" = (True, acc ++ (fmap (const "0") [1..l]), l)
      f (True, acc, l) "" = (True, acc, l)
      f (t, acc, l) r | not $ null r = (t, acc ++ [r], l)
      toDecimal s = snd $ foldr g (0, 0) s
      g l (i, r) = (i + 1, r + (fromHex $ toLower l) * 16 ^ i)
      sum' s = snd $ foldr h (0, 0) s
      h l (i, r) = (i + 1, r + l * 16 ^ (i * 4))
      m ws = sum' $ fmap (fmap toDecimal) uncompressed ws
      fromHex '0' = 0
      fromHex '1' = 1
      fromHex '2' = 2
      fromHex '3' = 3
      fromHex '4' = 4
      fromHex '5' = 5
      fromHex '6' = 6
      fromHex '7' = 7
      fromHex '8' = 8
      fromHex '9' = 9
      fromHex 'a' = 10
      fromHex 'b' = 11
      fromHex 'c' = 12
      fromHex 'd' = 13
      fromHex 'e' = 14
      fromHex 'f' = 15

-- Fuck it
