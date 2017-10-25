-- ExBuildingFunctions.hs

module ExBuildingFunctions where

curryStr :: String
curryStr = "Curry is awesome!"

shout :: String -> String
shout x = x ++ "!"

fifthElement :: String -> Char
fifthElement x = x !! 4

dropNine :: String -> String
dropNine = drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 4

letterFromCurryStr :: Int -> Char
letterFromCurryStr x = curryStr !! x

rvrs :: String
rvrs = (drop 9 str) ++ " " ++ (take 3 $ drop 6 str) ++ (take 5 str)
  where str = "Curry is awesome"
