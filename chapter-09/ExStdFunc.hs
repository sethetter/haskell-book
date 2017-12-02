module ExStdFunc where

-- 1.

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_) = True
myOr (x:xs) = x || myOr xs

-- 2.

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x       = True
  | otherwise = myAny f xs

-- 3.

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
  | e == x    = True
  | otherwise = myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (== e)

-- 4.

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5.

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

-- 6.

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7.

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy _ [] = Nothing
myMaximumBy f (x:xs) = go x (x:xs)
  where go m []     = Just m
        go m (x':xs') =
          if f x' m == GT then go x' xs'
                          else go m xs'

-- 9.

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy _ [] = Nothing
myMinimumBy f (x:xs) = go x (x:xs)
  where go m []     = Just m
        go m (x':xs') =
          if f x' m == LT then go x' xs'
                          else go m xs'

-- 10.

myMaximum :: (Ord a) => [a] -> Maybe a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> Maybe a
myMinimum = myMinimumBy compare
