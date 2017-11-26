-- WordNumber.hs

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "Zero"
digitToWord 1 = "One"
digitToWord 2 = "Two"
digitToWord 3 = "Three"
digitToWord 4 = "Four"
digitToWord 5 = "Five"
digitToWord 6 = "Six"
digitToWord 7 = "Seven"
digitToWord 8 = "Eight"
digitToWord 9 = "Nine"
digitToWord _ = "Invalid"

digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber n =
  concat $ intersperse "-" $ map digitToWord (digits n)
