module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

sing2 :: [Char]
sing2 = if (x < y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
