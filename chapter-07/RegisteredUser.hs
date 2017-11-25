module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User
  = RegisteredUser Username AccountNumber
  | UnregisteredUser

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "Unregistered User"
printUser (RegisteredUser
          (Username un)
          (AccountNumber an)) =
  putStrLn $ un ++ "(" ++ (show an) ++ ")"
