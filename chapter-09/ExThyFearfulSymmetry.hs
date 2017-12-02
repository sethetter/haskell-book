-- ExThyFearfulSymmetry.hs

module ExThyFearfulSymmetry where

-- 1.

myWords :: String -> [String]
myWords ""      = []
myWords (' ':xs) = myWords xs
myWords s = [firstWord] ++ myWords rest
  where firstWord = takeWhile (/= ' ') s
        rest      = dropWhile (/= ' ') s

-- 2.

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences =
  firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines ('\n':xs) = myLines xs
myLines s = [firstLine] ++ myLines rest
  where firstLine = takeWhile (/= '\n') s
        rest      = dropWhile (/= '\n') s

-- 3.

split :: Char -> String -> [String]
split _ "" = []
split c s
  | c == head s = split c $ tail s
  | otherwise = [first] ++ split c rest
     where first = takeWhile (/= c) s
           rest  = dropWhile (/= c) s

myWords' = split ' '
myLines' = split '\n'
