# Chapter 10: Folding Lists

Folds are also known as _catamorphisms_ . Cata = down, so "morphing down". They
are a way of taking the structure of a piece of structured data (list, or
otherwise), and deconstructing it down to something else.

A parallel between `map` and `foldr` : while `map` applies a function to each
member in a list and returns the list, `foldr` replaces the cons constructor
with the function passed in and then evaluates it down.

`foldr` is known as "fold right" because it associates to the right. Similarly,
`foldl` associates to the left.

All folds have two steps -- traversal over the spine, and then evaluation of the
folding function on these values. Both types of folds, left and right, traverse
in the same direction. The difference is in the association, or parenthesization
of the evaluation of the folding function.

`foldr` can be used on infinite lists, due to the right association of the
function. If the function being applied no longer calls for the evaluation of
the next argument (the rest of the fold), then it can stop and return that
result.

Example include `const` , or using `foldr` to implement `any` . The `False` base
case is returned continuously, until a `True` is hit, which stops the evaluation
of the right side of `||` .

    myAny f x = case of
    	[] -> False
     (x:xs) -> (f x || foldr f False xs)	

Regarding evaluation on a fold — the book states that like in other scenarios
benefiting from lazy evaluation, in a fold, we can have a bottom in the list
without issue if we're sure it will never need to evaluate it (based on the fold
function or otherwise). However, if a bottom is the first piece _of the spine_ ,
then it will always be forced to evaluate because it has to be able to determine
the first cons cell always. It might not need what's _in_ the cell, but the cell
itself can also be a bottom.

This is a little bit confusing, but oh well? I get it in theory, I just don't
understand how a cons **cell** could ever be a bottom in a practical scenario.
But good to know regardless!

Scans can be used to see the evaluation of a fold. It does the same thing but
returns a list with all the intermediary steps along the way, instead of just
the final result.

    Prelude Data.List> scanr (+) 0 [1..5]
    [15,14,12,9,5,0]
    Prelude Data.List> scanl (+) 0 [1..5]
    [0,1,3,6,10,15]

## Exercises: Understanding Folds

1. `(foldr (*) 1 [1..5]) == (foldl (flip (*)) 1 [1..5])`
2. Evaluation steps for `foldl (flip (*)) 1 [1..3]`
   - `foldl (flip (*)) 1 [1,2,3]`
   - `(foldl (flip (*)) 1 [1,2]) * 3`
   - `((foldl (flip (*)) 1 [1]) * 2) * 3`
   - `(((foldl (flip (*)) 1 []) *1) * 2) * 3`
   - `(((1 * 1) * 2) * 3`
3. C. `foldr`, but not `foldl`, associaties to the right.
4. Folds are catamorhpic, which means they (a) reduce structure.
5. Correct the expressions
  a. `foldr (++) "" ["woot", "WOOT", "woot"]`
  b. `foldr max '\0' "fear is  the little death"`
  c. `foldr (&&) True [False, True]`
  d. `foldr (||) True [False, True]`, no change here, because it runs fine
     already. Not sure what the intention was here.
  e. `foldl (flip $ (++) . show) "" [1..5]`
  f. `foldr const 'a' [1..5]`
  g. `foldr const 0 "tacos"`
  h. `foldr (flip const) 0 "burritos"`
  i. `foldr (flip const) 'z' [1..5]`

`foldl` is generally not good with infinite lists because it must evaluate the
entire spine before evaluating any values. Because of this, it's pretty much
always better to use `foldl'` instead of `foldl`, so you don't take a
performance hit on large lists for holding intermediate expressions.

## Exercises: Database Processing

`ExDatabaseProc.hs`

## Scans

Scans show the intermediate values created when folding a list, and returns a
list of all of those values.

    foldr :: (a -> b -> b) -> b -> [a] -> b
    scanr :: (a -> b -> b) -> b -> [a] -> [b]
    
    foldl :: (b -> a -> b) -> b -> [a] -> b
    scanl :: (b -> a -> b) -> b -> [a] -> [b]

Technically, scans are not catamorphisms, since they result in a list.

## Scan Exercises

1. `take 20 fibs`
2. `takeWhile (<100) fibs`
3. `let fac = scanl (*) 1 [1..]`
  a. Or a non-infinite version -> `let fac n = foldr (*) 1 [1..n]`
