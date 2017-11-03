module ExCorrectingSyntax where

-- 1. Write a function that adds 1 to the length of a string.

x = (+)

f xs = w `x` 1
  where w = length xs

-- 2. This is supposed to be the identity function, `id`.

f' x = x

-- 3. Should return `1` from `(1, 2)`.

f'' (a, b) = a
