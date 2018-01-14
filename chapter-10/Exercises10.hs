module Exercises10 where

-- Warm-up and Review

-- 1.

stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

-- 1a.

abaPerms :: [a] -> [a] -> [(a, a, a)]
abaPerms l1 l2 =
  concatMap
    (\x ->
       concatMap
         (\y ->
            map (\x' -> (x, y, x')) l1
         ) l2
    ) l1

svsPerms :: [(Char, Char, Char)]
svsPerms = abaPerms stops vowels

-- 1b.

svsPerms' :: [(Char, Char, Char)]
svsPerms' = filter (\(x, _, _) -> x == 'p') svsPerms

-- 1c.

nouns, verbs :: [String]
nouns = ["giraffe", "yogurt", "bicycle", "toenail"]
verbs = ["caressed", "ate", "punched", "belittled"]

nvnPerms :: [(String, String, String)]
nvnPerms = abaPerms nouns verbs

-- 2.

-- Gets the average length of all words in a string
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- 3.

seekritFunc' :: Fractional a => String -> a
seekritFunc' x =
  (/) (fromIntegral . sum $ map length $ words x)
      (fromIntegral . length $ words x)

-- Rewriting functions using folds

-- 1.

myOr :: [Bool] -> Bool
myOr = foldr (flip (||)) False

-- 2.

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x r -> r || f x) False

-- 3.

myElem :: Eq a => a -> [a] -> Bool
myElem n = foldr (\x r -> r || x == n) False

-- 4.

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

-- 6.

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

-- 7.

squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

-- 9.

squishAgain :: [[a]] -> [a]
squishAgain =  squishMap id

-- 10.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\i acc -> if f i acc == GT then i else acc)

-- 11.

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\i acc -> if f i acc == LT then i else acc)
