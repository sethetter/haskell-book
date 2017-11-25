-- greetIfCool.hs

module GreetIfCool where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True  ->
      putStrLn "eyyyy. What's shakin'?"
    False ->
      putStrLn "pshhh."
  where cool = coolness == "downright frosty yo"
