Exercises, Chapter 08
====

Review Of Types
----

1. d. `[[Bool]]`
2. b. `[[3 == 3], [6 > 5], [3 < 4]]`
3. d. All of the above.
4. b. `func "Hello" "World"`

Reviewing Currying
----

> cattyConny :: String -> String -> String
> cattyConny x y = x ++ " mrow " ++ y
>
> flippy :: String -> String -> String
> flippy = flip cattyConny
>
> appedCatty = cattyConny "woops"
> frappe = flippy "haha"

1. `"woops mrow woohoo!"`
2. `"1 mrow haha"`
3. `"woops mrow 2 mrow haha"`
4. `"woops mrow blue mrow haha"`
5. `"pink mrow green mrow woops mrow blue"`
6. `"are mrow Pugs mrow awesome"`

Recursion
----

1.
< dividedBy 15 2 =
< go 15 2 0
< go (15 - 2) 2 (0 + 1)
<
< go 13 2 1
< go (13 - 2) 2 (1 + 1)
<
< go 11 2 2
< go (11 - 2) 2 (2 + 1)
<
< go 9 2 3
< go (9 - 2) 2 (3 + 1)
<
< go 7 2 4
< go (7 - 2) 2 (4 + 1)
<
< go 5 2 5
< go (5 - 2) 2 (5 + 1)
<
< go 3 2 6
< go (3 - 2) 2 (6 + 1)
<
< go 1 2 7
< 1 < 2 == True
< => (7, 1)

2.

> sumR :: (Ord a, Num a) => a -> a
> sumR n
>   | n <= 1    = 1
>   | otherwise = n + sumR (n - 1)

3.

> multR :: (Integral a) => a -> a -> a
> multR x y
>   | y == 1    = x
>   | otherwise = x + (multR x (y - 1))

Fixing dividedBy
----

> data DividedResult
>   = Result Integer
>   | DividedByZero
>   deriving (Show)
>
> dividedBy :: Integer -> Integer -> DividedResult
> dividedBy num denom = go (abs num) (abs denom) 0
>   where go n d count
>          | d == 0    = DividedByZero
>          | n < d     = result count
>          | otherwise = go (n - d) d (count + 1)
>         result count
>          | num < 0 && denom < 0 = Result count
>          | num > 0 && denom > 0 = Result count
>          | otherwise            = Result (- count)

McCarthy 91 Function
----

> mc91 :: (Ord a, Num a) => a -> a
> mc91 n
>   | n > 100   = n - 10
>   | otherwise = mc91 $ mc91 $ n + 11

Numbers into Words
----

In the `WordNumber.hs` file.
