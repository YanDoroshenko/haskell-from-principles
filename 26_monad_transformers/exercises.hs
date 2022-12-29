{-# LANGUAGE OverloadedStrings #-}
module Exercises where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (get)
import qualified Control.Monad.Trans.State (get)
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import System.Random (randomRIO)
import Web.Scotty.Trans hiding (put)

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . (flip (-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
  _ <- putStrLn $ "Hi: " ++ show a
  return $ a + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> do
  let showA = show a
  _ <- putStrLn $ "Hi: " ++ show a
  return (showA, a + 1)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
  guard $ isValid v
  return $ Just v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn
      ("Good, was very excite: " ++ e)

data Config = Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
}

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
  -> M.Map Text Integer
  -> (M.Map Text Integer, Integer)
bumpBoomp k m = case M.lookup k m of
                  Nothing -> (M.insert k 1 m, 1)
                  Just v -> (M.adjust (+1) k m, v + 1)

app :: Scotty ()
app =
  get "/:key" $ do
    (unprefixed :: Text) <- param "key"
    p <- lift $ asks prefix
    c <- lift $ asks counts
    c' <- liftAndCatchIO $ readIORef c
    let key' = mappend p unprefixed
        (newM, newInteger) = bumpBoomp key' c'
    _ <- liftAndCatchIO $ writeIORef c newM
    html $ mconcat [ "<h1>Success! Count was: "
      , TL.pack $ show newInteger
      , "</h1>"
      ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR m = runReaderT m config
  scottyT 3000 runR app

morra :: StateT (Int, Int) IO ()
morra = fmap (const ()) $ iterateWhile isJust $ do
  lift $ putStrLn "Enter 1 or 2"
  (p, c) <- Control.Monad.Trans.State.get
  p' <- lift $ getLine
  c' <- lift $ (randomRIO (1, 2) :: IO Int)
  case (p', c') of
    ("1", 1) -> do
      let s' = (p + 1, c)
      put s'
      return $ Just s'
    ("2", 1) -> do
      let s' = (p, c + 1)
      put s'
      return $ Just s'
    ("1", 2) -> do
      let s' = (p, c + 1)
      put $ s'
      return $ Just s'
    ("2", 2) -> do
      let s' = (p, c + 1)
      put s'
      return $ Just s'
    _ -> do
      lift $ putStrLn $ "The score is: player " ++ (show p) ++ ", computer " ++ (show c)
      return Nothing
