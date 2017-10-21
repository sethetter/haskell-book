-- practice.hs

module Practice where

-- Exercise 1 on bottom of page 59
expr1 = x * 3 + 1
  where x = 3
        y = 1000

-- Exercise 2 on bottom of page 59
expr2 = x * 5
  where y = 10
        x = 10 * 5 + y

-- Exercise 3 on top of page 60
expr3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

triple x = x * 3

waxOff x = triple $ x + 1
