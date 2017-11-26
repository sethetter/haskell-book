Recursion
====

Recursion is self-referential function composition! Instead of having a fixed
number of applications that happen though, we instead set limits within the
function definition on when to stop adding additional applications.

> applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
> applyTimes 0 f b = b
> applyTimes n f b = f (applyTimes (n - 1) f b)

Intermission: Exercise
----

< applyTimes 5 (+1) 5
< (+1) $ applyTimes 4 (+1) 5
< (+1) $ (+1) $ applyTimes 3 (+1) 5
< (+1) $ (+1) $ (+1) $ applyTimes 2 (+1) 5
< (+1) $ (+1) $ (+1) $ (+1) $ applyTimes 1 (+1) 5
< (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ applyTimes 0 (+1) 5
< (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ 5

Fibonacci Numbers
----

> fibonacci 0 = 0
> fibonacci 1 = 1
> fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

Integral Division (w/ Recursion!)
----

> dividedBy :: Integral a => a -> a -> (a, a)
> dividedBy num denom = go num denom 0
>   where go n d count
>          | n < d = (count, d)
>          | otherwise = go (n - d) d (count + 1)
