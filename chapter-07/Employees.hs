module Employees where

data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Show, Ord)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $
  show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee
             -> Employee
             -> IO ()
employeeRank f e e' = case f e e' of
  GT -> reportBoss e e'
  LT -> reportBoss e' e
  EQ -> putStrLn "Nobody is the boss!"

codersFtw :: Employee -> Employee -> Ordering
codersFtw Coder Coder = EQ
codersFtw Coder _     = GT
codersFtw _     Coder = LT
codersFtw e     e'    = compare e e'
