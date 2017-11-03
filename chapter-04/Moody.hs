module Moody where

data Mood = Blah | Woot deriving (Show, Eq)

instance Ord Mood where
  compare Woot Blah = GT
  compare Blah Woot = LT
  compare Woot Woot = EQ
  compare Blah Blah = EQ

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
