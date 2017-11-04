# Chapter 5: Types

Things to be covered in this chapter:

* More on querying and reading type signatures
* Learn about currying, the concept, not the food
* Look more at polymorphism, and different types therein
* Check out type inference, and writing types for our functions

Haskell is an implementation of a typed lambda calculus, based on research done
in computer science, mathematics, and logic have all lead to "System F", which
Haskell builds upon.

## Type signatures

The `->` symbol is the type constructor for functions. It is an infix operator,
and has the type of `(->) t1 t2`. So it's a type constructor that takes two
arguments (type arguments, mind you). The result is a type expression that
describes a function taking and argument and evaluating to a result.

This differs from the function itself in that the function syntax (f x = x) is
the **value** of that function type. Functions are values!

## Typeclasses

Typeclass constraints are expressed like so:

```
Num a => a -> Bool
```

This means that `a` is a constrained polymorphic type variable. Polymorphic
means there are multiple possibilities for which type it could represent.
Constrained means those possibilities are constrained to ones that implement the
`Num` typeclass.

Multiple constraints looks like this:

```
(Num a, Ord a) => a -> Bool
(Num a, Num b) => a -> b -> Bool
```

## Exercises: Type Matching

```
not :: Bool -> Bool
length :: [a] -> Int
concat :: [[a]] -> [a]
head :: [a] -> a
(<) :: Ord a => a -> a -> Bool
```

## Currying

A "curried" function is a function that only accepts one argument and returns
one result, though that result may be another function accepting one argument.
It creates the illusion of multiple argument functions while allowing for
conveniences like being able to *partially apply* the function, that is, give
the function only part of it's required arguments at first, and the rest at a
later time, potentially multiple times.

Because the function type constructor, `->` is an infix operator and is right
associative, currying is implicit in Haskell.

```
Int -> Int -> Bool -> Bool
-- same as..
Int -> (Int -> (Bool -> Bool))
```

The type signature for `->` is `(->) a b`, where `a` represents the input type
to the function, and `b` represents the type that the function will evaluate to.

In the case of multi-argument functions, it is in actuality just returning another
function with the first argument applied, as shown above.

## Sectioning

Sectioning is a technique that involves partially applying infix operator
functions. It has a special syntax that let's you choose whether to partially
apply the first or the second argument.

## Exercises: Type Arguments

1. a
  ```
  f :: a -> a -> a -> a
  x :: Char
  f x :: Char -> Char -> Char
  ```
2. d
  ```
  g :: a -> b -> c -> b
  g 0 'c' "woot" :: Char
  ```
3. d
  ```
  h :: (Num a, Num b) => a -> b -> b
  h 1.0 2 :: Num b => b
  ```
4. a
  ```
  h :: (Num a, Num b) => a -> b -> b
  h 1 (5.5 :: Double) :: Double
  ```
5. a
  ```
  jackal :: (Ord a, Eq b) => a -> b -> a
  jackal "keyboard" "has the word jackal in it" :: [Char]
  ```
6. e
  ```
  jackal :: (Ord a, Eq b) => a -> b -> a
  jackal "keyboard" :: Eq b => b -> [Char]
  ```
7. d
  ```
  kessel :: (Ord a, Num b) => a -> b -> a
  kessel 1 2 :: (Num a, Ord a) => a
  ```
8. a
  ```
  kessel :: (Ord a, Num b) => a -> b -> a
  kessel 1 (2 :: Integer) :: (Num a, Ord a) => a
  ```
9. c
  ```
  kessel :: (Ord a, Num b) => a -> b -> a
  kessel (1 :: Integer) 2 :: Integer
  ```
  
## Polymorphism

Polymorphic type variables are variables in type signatures that can be
inhabited by a variable number of types. There are three main kinds of types:
concrete, meaning it has a specific type; constrained polymorphic, meaning it's
within a limited set of types based on typeclasses; and parametric polymorphic
meaning it could be any type.

## Exercises: Parametricity

1. In `GHC.Exts` there are two other functions with type `a -> a`, but they
   *both* resolve to `id` after performing some compiler trick.
2. Giving non-matching arguments results in a type error.
  ```
  f, f' :: a -> a -> a
  f x y = x
  f' x y = y
  ```
3. Behavior does not change with changed types.
  ```
  g :: a -> b -> b
  g x y = y
  ```

## Polymorphic Constants

In contrast to polymorphic type variables, a polymorphic constant is a value
that can be represented as any type that implements the typeclass constraints
set on it. An example is `10`, which has a type of `Num a => a`. This means that
the concrete value `10` can be used as any type that implements `Num`.

## Type Inference 

The compiler is capable of figuring out the possible types of variables based on
the operations performed with them and how they are used in other functions. It
will always assert the most general type declaration that it can.

## Exercises: Apply Yourself

1. `" yo"` is `[Char]`, so the `a` type variable becomes concrete to this.
  ```
  (++) :: [a] -> [a] -> [a]
  myConcat :: [Char] -> [Char]
  myConcat x = x ++ " yo"
  ```
2. Because the `/` operation results in a `Fractional`, the `a` type variable
  becomes more specific, or less polymorphic.
  ```
  (*) :: Num a => a -> a -> a
  myMult :: Fractional a => a -> a
  myMult x = (x / 3) * 5
  ```
3. `Char` is the concrete type used for `a`, so becomes more specific to this. 
  ```
  take :: Int -> [a] -> [a]
  myTake :: Int -> [Char]
  myTake x = take x "hey you"
  ```
4. Because the right side of `>` is `Int`, this is used as conrete type for `a`.
  ```
  (>) :: Ord a => a -> a -> Bool
  myCom :: Int -> Bool
  myCom x = x > (length [1..10])
  ```
5. Same as previous, except `Char` here.
  ```
  (<) :: Ord a => a -> a -> Bool
  myAlph :: Char -> Bool
  myAlph x = x < 'z'
  ```

## Asserting types for declarations

It's generally a good idea and a best practice to include type signatures with
your function definitions. While type inference is good, there will often be
cases where you want to enforce a more specific type in a place where the
compiler infers something more general.

# Chapter Exercises

## Multiple Choice

1. C
2. A
3. B
4. C

## Determine the type

1.
  a. `(* 9) 6 :: Num a => a`
  b. `head [(0, "doge"), (1, "kitteh")] :: Num a => (a, [Char])`
  c. `head [(0 :: Integer, "doge"), (1, "kitteh")] :: (Integer, [Char])`
  d. `if False then True else False :: Bool`
  e. `length [1, 2, 3, 4, 5] :: Int`
  f. `(length [1, 2, 3, 4]) > (length "TACOCAT") :: Bool`
2. `Num a => a`
3. `Num a => a -> a`
4. `Fractional a => a`
5. `[Char]`

## Does it compile?

1. The `wahoo` line is problemative. `bigNum` is not a function, because `(^)`
  has all arguments applied. We can fix it like so:
  ```
  bigNum = (^) 5
  wahoo = bigNum 10
  ```
2. All of these are fine, if maybe a bit strange.
  ```
  x = print
  y = print "woohoo!"
  z = x "hello world"
  ```
3. Issue on `c = b 10`, because `b` is not a function. Fixed:
  ```
  a = (+)
  b = 5
  c = a 10
  d = c 200
  ```
4. There is no `c` defined here, so if we define one it's fixed!
  ```
  a = 12 + b
  b = 10000 * c
  c = 5
  ```
  
## Type variable or specific type constructor?

1. 0: constrained to `Num`, 1: fully polymorphic, 3: concrete, 4: concrete
2. `zed`: fully polymoprhic, `Zed`: concrete, `Blah`: concrete
3. `a`: fully polymorphic, `b`: constrained to `Enum`, `C`: concrete
4. `f`: fully polymorphic, `g`: fully polymorphic, `C`: concrete


## Write a type signature

1. 
  ```
  functionH :: [a] -> a
  functionH (x:_) = x
  ```
2.
  ```
  functionC :: Ord a => a -> a -> Bool
  functionC x y = if (x > y) then True else False
  ```
3.
  ```
  functionS :: (a, b) -> b
  functionS (x, y) = y
  ```

## Given a type, write the function

1.
  ```
  i :: a -> a
  i = id
  ```
2.
  ```
  c :: a -> b -> a
  c x _ = x
  ```
3. Yes
4.
  ```
  c' :: a -> b -> b
  c' _ = id
  ```
5.
  ```
  r :: [a] -> [a]
  r = cycle
  r' = reverse
  r'' = init
  r''' = tail
  ```
6.
  ```
  co :: (b -> c) -> (a -> b) -> a -> c
  co = (.)
  ```
7.
  ```
  a :: (a -> c) -> a -> a
  a _ = id
  ```
8.
  ```
  a' :: (a -> b) -> a -> b
  a' f = f
  ```
  
## Fit It

1. `Sing.hs`
2. `Sing.hs` (the `sing2` function)
3. `Arith3Broken.hs`

## Type-Kwon-Do

1. `h = g . f`
2. `e = w . q`
3. `xform (x, y) = (xz x, yz y)`
4. `munge f g = fst . g . f`
