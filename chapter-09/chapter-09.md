# Chapter 9: Lists

Lists in Haskell serve two purposes. 

- Hold collections of similarly typed values.
- Generate an infinite series of values defined by a function.

## The list datatype

`data [] a = [] | a : [a]`

- Lists are a recursive data structure.
- Sum type of two data constructors
  - `[]`, an empty list
  - `a : [a]`, a value **and** a list of same-typed values
- `:` is like **and**.

## Pattern matching on lists

Done just like other data types.

```
f :: [a] -> a
f (x:_) = x
-- Note: No case for `[]`
```

Match style of data constructor in data type.

To fix above example, use `Maybe`.

```
f :: [a] -> Maybe a
f [] = Nothing
f (x:_) = Just x
```

## List's syntactic sugar

- Normal: `(1 : 2 : 3 : []) ++ 4 : []`
- Sugar: `[1, 2, 3] ++ [4]`

Helps to avoid extra syntax for recursive data structure.

## Ranges for list construction

Syntax for generating lists using `enumFromTo` and `enumFromThenTo`.

- `[1..10] == (enumFromTo 1 10)`
- `[1,2..10] == (enumFromThenTo 1 2 10)`

## Exercise: EnumFromTo

`EnumFromTo.hs`

## Extracting portions of lists

- 
  ```
  take :: Int -> [a] -> [a]
  ```
  - Return first `x` number of elements from the list.
- 
  ```
  drop :: Int -> [a] -> [a]
  ```
  - Drop the first `x` number of elements and return remaining.
-
  ```
  splitAt :: Int -> [a] -> ([a], [a])
  ```
  - Split list after `x` number, and return tuple of lists with split.
-
  ```
  takeWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile :: (a -> Bool) -> [a] -> [a]
  ```
  - These both take or drop from a list until it meets an element which
    evaluates to `False` when given to the function provided.

## Exercises: Thy Fearful Symmetry

`ExThyFearfulSymmetry.hs`

## List comprehensions

-
  ```
  [ x^2 | x <- [1..10]]
  ```
  - Defines an input and output separated by the pipe.
    - Output (left): function applied to members coming out of the list.
    - Input (right): a list from which to take arguments for the output function
- Can also take predicates:
  - `[x^2 | x <- [1..10], rem x 2 == 0]`
  - Members of input source must meet `True` for all listed predicates.
- Can also have multiple generators:
  - `[x^y | x <- [1..5], y <- [2, 3]]`
  - Rightmost generator exhausted first, meaning combinations run through first 
    leftmost generator value with all possible rightmost values, then onward.
- Can even generate tuples!
  -
    ```
    [(x, y) | x <- [1,2,3], y <- [6,7]]
    ```
- Also possible to use lists constructed from comprehensions as inputs to other
  list comprehensions.
  -
    ```
    mySqr = [x^2 | x <- [1..10]]
    [(x,y) | x <- mySqr, y <- [5,6,7], x < 4]
    ```

## Exercises: Comprehend Thy Lists

```
[ x | x <- mySqr, rem x 2 == 0]
-- => [4, 16, 36, 64, 100]
```

```
[ (x, y) |
  x <- mySqr,
  y <- mySqr,
  x < 50, y > 50 ]
-- => [(1, 64), (1, 81), (1, 100)
-- => ,(4, 64), (4, 81), (4, 100)
-- => ,(9, 64), (9, 81), (9, 100)
-- => ,(16, 64), (16, 81), (16, 100)
-- => ,(25, 64), (25, 81), (25, 100)
-- => ,(36, 64), (36, 81), (36, 100)
-- => ,(49, 64), (49, 81), (49, 100)]
```

## Using list comprehensions as functions

```
let acro xs = [ x | x <- xs, elem x ['A'..'Z'] ]
```

## Exercises: Square Cube

Given..

```
let mySqr = [ x^2 | x <- [1..5] ]
let myCube = [ x^3 | x <- [1..5] ]
```

1. `[ (x, y) | x <- mySqr, y <- myCube ]`
2. `[ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]`
3. `length [ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]`

## Nonstrict evaluation

- Haskell does not evaluate values until they are needed
  - This applies to lists with ranges or comprehensions
- Using GHCi's `:sprint` command shows us what of an expression has been
  evaluated, with `_` representing unevaluated.
  -
     ```
     let blah = ['a'..'z']

     :sprint blah
     -- => blah = _
     
     take 2 blah
     -- => "ab"
     
     :sprint blah
     -- => blah = 'a' : 'b' : _
     ``` 

## Spines

- The `length` function is only strict on the spine of a list, not the values.
  - However, `:sprint` will show the list fully evaluated when using `length`.
  - This is one of the quirks of GHCi, unfortunately.
- The **spine** represents the `cons` operators that are used in the data
  constructors that make up a list.
  - Data constructors are accompanied by a value, but that value does not have
    to be evaluated due to Haskell's nonstrict nature.
  - 
    ```
    let foo = [1..5]
    take 3 foo
    :sprint foo
    -- => _ : _ : _ : _
    ```
    - Except `:sprint` won't really work this way..

## Exercises: Bottom Madness
 
1. Bottom
2. `[1]`
3. Bottom
4. `3`
5. Bottom
6. `[2]`
7. Bottom
8. `[1]`
9. `[1,3]`
10. Bottom

## Is it in normal form?

1. NF
2. WHNF
3. Neither
4. Neither
5. Neither
6. Neither
7. WHNF

Not feeling super confident on the above answers.

## Transforming lists of values

Use `map` to apply a function to all items in a `[]`. `fmap` is the `Functor`
based version of this.

Laziness rules apply here, `map` does not calculate the result of apply the
function to the cell until it is requested.

## Exercises: More Bottoms

1. `take 1 $ map (+1) [undefined, 2, 3]`: Bottom
2. `take 1 $ map (+1) [1, undefined, 3]`: `[1]`
3. `take 2 $ map (+1) [1, undefined, 3]`: Bottom
4. `itIsMystery xs = map (\x -> elem x "aeiou") xs`, type of `[Char] -> [Bool]`,
   returns a list for each character, `True` if that character is a vowel.
5. Result of following functions?
  a. `map (^2) [1..10]`: `[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]`
  b. `map minimum [[1..10], [10..20], [20..30]]`: `[1, 10, 20]`
  c. `map sum [[1..5], [1..5], [1..5]]`: `[15, 15, 15]`
6. `map (\x -> bool x (-x) $ x == 3) [1..10]`

## Filtering lists of values

Another common action performed on lists is conditionally filtering out values.
The function used for this is conveniently called `filter`.

Filtering can also be done with list comprehensions, as we have seen.

## Exercises: Filtering

1. `let multsOfThree = filter (\x -> (rem x 3) == 0)`
2. `let howManyMultsOfThree = length . multsOfThree`
3. ...
   ```
   let removeArticles = (filter $ not . (`elem` ["a", "an", "the"])) . words
   ```

## Zipping lists

Zipping is the process of combining multiple lists into a single list.

By default, `zip` results in a list of tuples of values in corresponding indices
from the two input lists.

```
zip [1,2,3] [4,5,6] => [(1,4),(2,5),(3,6)]
```

`zip` stops as soon as either list runs out of values. You can use this as a
trick to combine `zip` with infinite lists.

`unzip` will take a zipped list and return a tuple of the lists separated again.

Due to `zip` stopping at the end of the shortest list, zipping and then
unzipping two lists can result in lost data.

`zipWith` accepts a function `(a -> b -> c)` that can be used to handle the
combination of list elements. Note that `(,) :: a -> b -> (a, b)`.

## Zipping exercises

1. ...
  ```
  zip' :: [a] -> [b] -> [(a, b)]
  zip' [] _ = []
  zip' _ [] = []
  zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
  ```
2. ...
  ```
  zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith' _ [] _ = []
  zipWith' _ _ [] = []
  zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys
  ```
3. ...
  ```
  zip'' = zipWith' (,)
  ```


# Chapter Exercises

`ExDataChar.hs`, `ExCipher.hs` and `ExStdFunc.hs`
