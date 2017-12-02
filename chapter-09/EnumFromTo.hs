module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool True True   = [True]
eftBool False False = [False]
eftBool True False  = []
eftBool False True  = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftOrdEnum

eftInt :: Int -> Int -> [Int]
eftInt = eftOrdEnum

eftChar :: Char -> Char -> [Char]
eftChar = eftOrdEnum

eftOrdEnum :: (Ord a, Enum a) => a -> a -> [a]
eftOrdEnum x y
  | x > y     = x : eftOrdEnum (succ x) y
  | x == y    = [x]
  | otherwise = []
