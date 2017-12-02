module ExDataChar where

import Data.Char

{-
1.
  isUpper :: Char -> Bool
  toUpper :: Char -> Char
-}

-- 2.

removeLower :: String -> String
removeLower = filter isUpper

-- 3.

capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = (toUpper x) : xs

-- 4.

allUpper :: String -> String
allUpper = map toUpper

-- 5. & 6.

capitalizedFirst :: String -> Char
capitalizedFirst = toUpper . head
