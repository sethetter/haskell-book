module ExCipher where

import Data.Char

-- Create a `caesar` cipher function that accepts a shiftNum and
-- a string to encrypt and shifts each character by the shiftNum

caesar :: Int -> String -> String
caesar shiftNum = map (chr . shiftLetters . ord)
  where shiftLetters :: Int -> Int
        shiftLetters pos
          | pos `elem` [97..122] && newPos > 122 = 96 + (newPos - 122)
          | pos `elem` [65..91]  && newPos > 90  = 64 + (newPos - 90)
          | pos `elem` [65..91] ++ [97..122]     = newPos
          | otherwise                            = pos
          where newPos = pos + shiftNum

unCaesar :: Int -> String -> String
unCaesar = caesar . negate
