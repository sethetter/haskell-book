-- Lecture06.hs

module Lecture06 where

data Trivial
  = Trivial

instance Eq Trivial where
  Trivial == Trivial = True

-- ...

data DayOfWeek
  = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

data Date
  = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  -- (==) _   _   = False

instance Eq Date where
  (==) (Date dayOfWeek dayOfMonth)
       (Date dayOfWeek' dayOfMonth')
    = dayOfWeek == dayOfWeek' && dayOfMonth == dayOfMonth'

-- When defining typeclass instances for datatypes that have a
-- type variable in them, you can define typeclass constraints on
-- those variables, and need to in some cases.
data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v')
    = v == v'


-- NOTE: Never define default values with typeclasses, this is only
-- for the purposes of demonstration.

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age
  = Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber = Age
  toNumber (Age n) = n
  defaultNumber = Age 27

newtype Year
  = Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber = Year
  toNumber (Year n) = n
  defaultNumber = Year 1990

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA  = toNumber a
        integerOfA' = toNumber a'
        summed = integerOfA + integerOfA'

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100
