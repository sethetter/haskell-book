module ExDatabaseProc where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbNumber 9003
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1.

getDates :: [DatabaseItem] -> [UTCTime]
getDates = foldl (\acc r -> case r of
                    DbDate t -> acc ++ [t]
                    _        -> acc
                 ) []

-- 2.
getInts :: [DatabaseItem] -> [Integer]
getInts = foldl (\acc r -> case r of
                    DbNumber i -> acc ++ [i]
                    _        -> acc
                 ) []

-- 3.

mostRecentDate :: [DatabaseItem] -> UTCTime
mostRecentDate = foldl (\mostRecent r -> case r of
                    DbDate t -> if mostRecent < t then t else mostRecent
                    _        -> mostRecent
                 ) $ UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 0)

-- 4.

-- Did a different style here.
sumDb :: [DatabaseItem] -> Integer
sumDb = foldl f 0
  where f :: Integer -> DatabaseItem -> Integer
        f s (DbNumber x) = s + x
        f s _            = s

-- 5.

avgDb :: [DatabaseItem] -> Double
avgDb = uncurry (/) . foldr (\dbItem (s, c) -> case dbItem of
                  DbNumber x -> (s + (fromIntegral x), c + 1.0)
                  _          -> (s, c)
              ) (0.0, 0.0)
