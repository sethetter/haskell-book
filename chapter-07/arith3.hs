-- arith3.hs

module Arith3 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

main :: IO ()
main = do
  print $ roundTrip 5
  print $ roundTrip' 6
  print (roundTrip'' 4 :: Int)
  print $ id 2
