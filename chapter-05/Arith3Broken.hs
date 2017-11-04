-- Arith3Broken.hs
module Arith3Broken where

main :: IO ()
main = do
  print (1 + 2 :: Integer)
  putStrLn "10"
  print (negate (-1) :: Integer)
  print ((+) 0 blah)
  where blah :: Integer
        blah = negate 1
