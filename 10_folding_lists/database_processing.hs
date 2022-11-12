module DatabaseProcessing where

import Data.Time
import Data.Maybe
import Control.Monad (mfilter)

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [
    DbDate (
      UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)
    ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbNumber 1,
    DbDate (
      UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
    )
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\l r -> case l of
                                DbDate time -> time : r
                                _ -> r
                     ) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\l r -> case l of
                                  DbNumber n -> n : r
                                  _ -> r
                       ) []

mostRecent :: [DatabaseItem] -> Maybe UTCTime
mostRecent = foldr (\l r -> case l of
                              DbDate time -> max' time
                                where max' time
                                        | isNothing $ mfilter (time <) r = Just time
                                        | otherwise = r
                              _ -> r
                   ) Nothing

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (\l r -> case l of
                     DbNumber n -> r + n
                     _ -> r
             ) 0

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (fst cnt) / (snd cnt)
  where cnt = foldr (\l r -> case l of
                               DbNumber n -> (fst r + n, snd r + 1)
                               _ -> r
                    ) (0, 0) db
