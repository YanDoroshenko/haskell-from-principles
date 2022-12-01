module WordNumberTest where

import Test.Hspec
import Data.List
import Data.Char
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.List (sort)
import Data.Maybe

digitToWord :: Int -> String
digitToWord n = case n of
                  0 -> "zero"
                  1 -> "one"
                  2 -> "two"
                  3 -> "three"
                  4 -> "four"
                  5 -> "five"
                  6 -> "six"
                  7 -> "seven"
                  8 -> "eight"
                  9 -> "nine"
                  _ -> ""

digits :: Int -> [Int]
digits 0 = []
digits n = (digits d) ++ [m]
  where
    (d, m) = divMod n 10

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ digitToWord <$> (digits n)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
  describe "half identity" $
    it "holds" $ property $ \x -> (halfIdentity x) == x
  describe "list sort" $
    it "is ordered" $ property $ (listOrdered . sort :: [String] -> Bool)
  describe "addition associativity" $
    it "holds" $ property (\(x :: Int) (y :: Int) (z :: Int) -> plusAssociative x y z)
  describe "addition commutativity" $
    it "holds" $ property (\(x :: Int) (y :: Int) -> plusCommutative x y)
  describe "multiplication associativity" $
    it "holds" $ property (\(x :: Int) (y :: Int) (z :: Int) -> multiplyAssociative x y z)
  describe "multiplication commutativity" $
    it "holds" $ property (\(x :: Int) (y :: Int) -> multiplyCommutative x y)
  describe "quote rem" $
    it "combines into x" $ property $ do
      x <- arbitrary :: Gen Int
      y <- arbitrary `suchThat` (/= 0) :: Gen Int
      return $ (quot x y) * y + (rem x y) == x
  describe "div mod" $
    it "combines into x" $ property $ do
      x <- arbitrary :: Gen Int
      y <- arbitrary `suchThat` (/= 0) :: Gen Int
      return $ (div x y) * y + (mod x y) == x
  describe "foldr (:) and (++)" $
    it "are equal" $ property $ do
      x <- arbitrary :: Gen [Int]
      y <- arbitrary :: Gen [Int]
      return $ (foldr (:) y x) == (x ++ y)
  describe "foldr (++) [] and concat" $
    it "are equal" $ property $ do
      x <- arbitrary :: Gen [[Int]]
      return $ (foldr (++) [] x) == (concat x)
  describe "show . read" $
    it "equals identity" $ property $
      \(x :: Int) -> (read $ show x) == x
  describe "idempotence" $ do
    it "holds for capitalization" $ property $
      \x -> (capitalizeWord x) == (fourTimes capitalizeWord x)
    it "holds for sorting" $ property $
      \(x :: String) -> (sort x) == (fourTimes sort x)

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord = fmap toUpper

half :: Double -> Double
half x = x / 2

halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x
multiplyAssociative x y z = x * (y * z) == (x * y) * z
multiplyCommutative x y = x * y == y * x

main' :: IO ()
main' = do
  quickCheck (\x -> (halfIdentity x) == x)
  quickCheck (\(x :: [Int]) -> listOrdered $ sort x)
  quickCheck (\(x :: Int) (y :: Int) (z :: Int) -> plusAssociative x y z)
  quickCheck (\(x :: Int) (y :: Int) -> plusCommutative x y)
  quickCheck (\(x :: Int) (y :: Int) (z :: Int) -> multiplyAssociative x y z)
  quickCheck (\(x :: Int) (y :: Int) -> multiplyCommutative x y)
  quickCheck (\(x :: Int) (y :: Int) -> (quot x y ) * y + (rem x y) == x)
  quickCheck (\(x :: Int) (y :: Int) -> (div x y ) * y + (mod x y) == x)

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen = oneof [return Fulse, return Frue]
foolGen' = frequency [(2, return Fulse), (1, return Frue)]

data Puzzle = Puzzle String [Maybe Char] [Char] deriving Eq

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar =
      zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
   putStrLn $ "Your guess was: " ++ [guess]
   case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
     (_, True) -> do
       putStrLn "You already guessed that character, pick something else!"
       return puzzle
     (True, _) -> do
       putStrLn "This character was in the word, filling in the word accordingly"
       return (fillInCharacter puzzle guess)
     (False, _) -> do
       putStrLn "This character wasn't in the word, try again."
       return (fillInCharacter puzzle guess)

checkFillInCharacter :: IO ()
checkFillInCharacter = hspec $ do
  describe "fillInCharacter" $ do
    it "fills in the last character" $ property $ do
      word <- sublistOf ['a'..'z']
      let tried = tail word
      let filledInSoFar = Just <$> ('_' : tried)
      c <- arbitrary `suchThat` (\x -> (elem x word) && not (elem x tried))
      return $ case fillInCharacter (Puzzle word filledInSoFar tried) c of
        (Puzzle w f c) -> all isJust f
    it "fills in the first character" $ property $ do
      word <- sublistOf ['a'..'z']
      c <- arbitrary `suchThat` flip elem word
      let filledInSoFar = (const Nothing) <$> word
      return $ case fillInCharacter (Puzzle word filledInSoFar []) c of
                 (Puzzle w f t) -> filter isJust f == [Just c]

checkHandleGuess = hspec $ do
  describe "handleGuess" $ do
    it "doesn't add an already tried character" $ monadicIO $ do
      tried <- pick $ sublistOf ['a'..'z']
      c <- pick $ arbitrary `suchThat` flip elem tried
      let p = Puzzle "" [] tried
      run $ (\x -> x == p) <$> handleGuess p c
    it "adds a correct character" $ monadicIO $ do
      word <- pick $ sublistOf ['a'..'z']
      tried <- pick $ sublistOf word
      c <- pick $ arbitrary `suchThat` (\x -> (elem x word) && (not $ elem x tried))
      let alreadyFilled = filled word tried []
      let p = Puzzle word alreadyFilled tried
      res <- run $ handleGuess p c
      return $ case res of
                 (Puzzle w f t) -> (elem (Just c) f) && (elem c t)
    it "doesn't add an incorrect character" $ monadicIO $ do
      word <- pick $ sublistOf ['a'..'z']
      tried <- pick $ sublistOf word
      c <- pick $ elements ['a'..'z'] `suchThat` (\x -> not $ elem x word)
      let alreadyFilled = filled word tried []
      let p = Puzzle word alreadyFilled tried
      res <- run $ handleGuess p c
      return $ case res of
                 (Puzzle w f t) -> (not $ elem (Just c) f) && (elem c t)
          where filled [] tried alreadyFilled = alreadyFilled
                filled (h : t) tried alreadyFilled
                  | elem h tried = filled t tried (alreadyFilled ++ [Just h])
                  | otherwise = filled t tried (alreadyFilled ++ [Nothing])

caesar :: Int -> String -> String
caesar n str = case str of
                 "" -> ""
                 h : t -> chr (ord 'a' + ((ord h - ord 'a' + n) `mod` 26)) : (caesar n t)

checkCaesar = hspec $ do
  describe "caesar" $
    it "decoding an encoded word produces the same word" $ property $ do
      word <- sublistOf ['a'..'z']
      n <- arbitrary
      return $ word == ((caesar n) . (caesar (-n))) word
