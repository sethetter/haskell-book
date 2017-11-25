# Chapter 7: More Functional Patterns

This chapter covers more patterns and concepts from functional languages that
Haskell provides. These concept all work to create a style of programming that
consists of composing together different functions that act on data structures
defined in our type information -- a very simple base!

## Arguments and parameters

In Haskell (and other functional languages), functions are first class citizens,
meaning they can be passed as arguments to other functions, used as values in
expressions, and can be returned from functions as results.

It's also the default for every function to be **curried**, meaning it always
takes a single input and returns a single output. Functions that accept multiple
arguments behind the scenes are actually taking one at a time. Each time a
single argument is applied, the return value of that is a **partially-applied**
function.

When an argument is applied to a function's parameter, it is **bound** to that
value that is provided.

Defining arguments for a function is done by placing identifiers for the
parameters between the name of the function and the `=` sign. Doing this changes
the type of the function to include a function application arrow.

Arguments for a function are available anywhere within that function definition.
You can also have `let` expressions to declare more variables. Those variables
are only available within the `let` expression.

Declaring a variable in a let expression that uses the same name as an argument
to the function (or another variable in a parent `let` expression) overwrites
the value of that variable in subsequent expressions. This is called
**shadowing**.

## Anonymous functions

An anonymous function is a function that is not bound to a particular
identifier, so it is without a name. This is done using the backslash syntax,
like so: `\x -> x + 1`.

## Exercises: Grab Bag

1. All are equivalent.
2. D: `Num a => a -> a -> a`
3a.
  ```
  addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \x -> x + 1
  ```
3b.
  ```
  addFive = \x y -> (if x > y then y else x) + 5
  ```
3c.
  ```
  mFlip f x y = f y x
  ```
  
The utility of anonymous functions often shows when passing them as single-use
arguments to other higher-order functions that accept a function as an argument.


## Pattern Matching

Pattern matching involves providing patterns to match expressions against to
determine an outcome in the evaluation, and in some cases binding variables to
the results of those successful matches.

It can include matching against just about anything in the language at the term
level. Even `undefined`, different literals, and list syntax. It's very often
used with data constructors for the purpose of changing behavior based on which
member of a sum type is being provided.

We define patterns in the term level of a function definition. To cover multiple
cases, we provide multiple definitions with different patterns. If the first
fails to match, it will try the next until all options are exhausted. A common
pattern is to have an "anything else" pattern which is denoted with an `_`.

```
isItTwoOrThree :: Integer -> Bool
isItTwoOrThree 2 = True
isItTwoOrThree 3 = True
isItTwoOrThree _ = False
```

If you provide a definition that does not match on all possible patterns, GHC by
default will not catch this, and it can result in a runtime exception. With the
proper warnings enabled in the compiler, though, it will catch them.

Pattern matching is also useful for pulling data out of data constructors. If we
have defined a data constructor that accepts parameters to make up it's
structure, when we pattern match on those data constructors, we are able to pull
those member values out by binding them to variable names.

```
data User
  = User String
  | Guest
  
greetUser :: User -> String
greetUser (User un) = "Hello, " ++ un ++ "!"
greetUser Guest     = "Hello, Guest!"
```

## Exercises: Variety Pack

1a. `(a, b) -> a`
1b. `String`, no, `k1` and `k3` are `Num a => a`
1c. `k3`
2.
  ```
  f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
  f (a, _, c) (d, _, f) = ((a, d), (c, f))
  ```

## Case expressions

Case expressions are another way that we can pattern match on different results
of expressions or values in our code.

```
f :: Num a => a -> String
f x = case x > 10 of
  True -> "word"
  False -> "nope"
```

You can match on data constructors the same as you would in a function
definition's pattern matching. It can also match on values, just the same.

## Exercises: Case Practice

1.
  ```
  functionC x y = case x > y
    True -> x
    False -> y
  ```
2.
  ```
  ifEvenAdd2 n = case even n of
    True -> n + 2
    False -> n
  ```
3.
  ```
  nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
  ```
  
## Higher Order Functions

In functional programming, the ability to pass functions around as arguments is
an important feature. To allow this in Haskell, our type signatures will
introduce parentheses.

```
foo :: (a -> b) -> a -> b
foo f x = f x
```

This tells us that the first argument to the function is itself a function that
takes a single argument of type `a`.

## Exercises: Artful Dodgy

```
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2
```

1. `1`
2. `11`
3. `22`
4. `21`
5. `12`
6. `11`
7. `21`
8. `21`
9. `22`
10. `31`
11. `23`

## Guards

Guards allow us to use statements that evaluate to booleans to write functions
that are nice and succinct. Instead of having a long chain of `if`/`else`
statements, these are much cleaner.

```
myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x
```

The `otherwise` case is reached if no other previous guard statement evaluates
to `True`. Guards are always evaluated sequentially, so the cases should be
ordered from most restrictive to least restrictive.

Functions can also be used in the guard statements, and values as well (though,
functions are also values), including ones defined in `where` clauses.

```
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100
```

Just as in pattern matching and case statements, it's important to cover all our
cases here. Having the proper warnings enabled will let you know.

## Exercises: Guard Duty

1. Since guards are checked in order from top to bottom, `otherwise` always evaluates.
2. The first truthy condition that is matched in a guard statement is used.
3. B. `True` when `xs` is a palindrome.
4. `[a]`
5. `[a] -> Bool`
6. C. Whether number is positive or negative.
7. `(Num a) => a`
8. `(Num a) => a -> a`

## Function Composition

The function composition operator is used to create a pipe for results of one
function into the input of another function. This compliments Haskell's other
functional components as well.

The composition operator is a `.`, and works like so:

```
(f . g) x == f (g x)
```

## Pointfree Style

Pointfree style is a way of defining functions without expressing information
about their arguments. You can combine functions together with the composition
operator.

```
sumFirstTwo = (sum . take 2)
sumFirstTwo [1,2,3,4] == 3
```

# Exercises

## Multiple Choice

1. D.
2. B.
3. D.
4. B.
5. A.

## Let's write code

1.
  a.
    ```
    tensDigit :: Integral a => a -> a
    tensDigit x = d
      where (_, d) = (x `divMod` 10)
    ```
  b. Yes
  c.
    ```
    hunsDigit :: Integral a => a -> a
    hunsDigit = (`div` 10) . (`mod` 100)
    ```
2.
  ```
  foldBool :: a -> a -> Bool -> a
  foldBool x y b = case b of
    False -> x
    True  -> y

  foldBool' :: a -> a -> Bool -> a
  foldBool' x y b
    | b == False = x
    | b == True  = y
  ```
3.
  ```
  g :: (a -> b) -> (a, c) -> (b, c)
  g f (x, y) = (f x, y)
  ```
4. `arith3.hs`
5. `arith3.hs`
6. `arith3.hs`
