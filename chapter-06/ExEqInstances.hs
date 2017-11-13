-- ExEqInstances.hs

module ExEqInstances where

-- 1.

data TisAnIntenger = TisAnIntenger Integer

instance Eq TisAnIntenger where
  (==) (TisAnIntenger x)
       (TisAnIntenger y) = x == y


-- 2.

data TwoIntegers = TwoIntegers Integer Integer

instance Eq TwoIntegers where
  (==) (TwoIntegers x  y )
       (TwoIntegers x' y')
    = x == x' && y == y'


-- 3.

data StringOrInt
  = TisAString String
  | TisAnInt Int

instance Eq StringOrInt where
  (==) (TisAString s) (TisAString s') = s == s'
  (==) (TisAnInt x)   (TisAnInt y)    = x == y
  (==) _              _               = False


-- 4.

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'


-- 5.

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'


-- 6.

data Which a = ThisOne a | ThatOne a

-- This feels verbose, but..
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) (ThisOne x) (ThatOne y) = x == y
  (==) (ThatOne x) (ThisOne y) = x == y


-- 7.

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x)   (Hello y)   = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _            _          = False
