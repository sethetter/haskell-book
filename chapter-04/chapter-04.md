# Chapter 4 -- Basic Datatypes

Haskell is heavily focused on the type system. Type systems for languages like
Haskell were created based on the mathematical study of set theory. Set theory
is the study of collections of objects.

The *type constructor* is the name of the type, and what is used in type
signatures. The *data constructors* are the values that inhabit the type, it is
an instance (if you will) of that type represented as a value (of that type).

```
data Thing = Shoe | Brick | Chair
       ^             ^
       |             |
   Type Constructor  |
              Data Constructors
```

## Exercises: Mood Swings

```
data Mood = Blah | Woot deriving Show
```

1. Type constructor - `Mood`
2. Possible values - `Blah` and `Woot`
3. The return type is a data constructor, not the type name as it should be be
4.
  ```
  changeMood Blah = Woot
  changeMood Woot = Blah
  ```
5. `Moody.hs`

**Integral numbers** are positive or negative whole numbers.
* `Int` - fixed precision integral
* `Integer` - non fixed integral

**Fractional numbers** are not whole.
* `Float` - single precision (32bit) floating point number
* `Double` - double precision (64bit) floating point number
* `Rational` - holds `Integer` values for numerator and denominator
* `Scientific` - almost arbitrary, coefficient as `Integer` and exponent as
  `Int`. Available in stack or cabal.
  
All above types are part of the `Num` typeclass.

The `Fractional` typeclass is a typeclass that requires it's instances to also
be have an instance of `Num`. Neat! Here's the syntax for that.

```
class Num a => Fractional a where
```

It's great because the syntax is consistent with how you would specify a
typeclass constraint in any other type signature.

In general, it's best to use arbitrary precision fractional number types instead
of their fixed-precision counterparts. This helps to avoid the problems
typically present in floating point arithmetic.

The `Ord` and `Eq` typeclasses have instances for many types.

* `Eq` means that separate values of the type can be compared for equality
* `Ord` means that values of the type can be put in some order

If trying to compare a list of values, the type of the values that the list
contains must be orderable (have an instance of the `Ord` typeclass).

## Exercises: Find the mistakes

1. `not True && true` -> `not True && True`
2. `not (x = 6)` -> `not (x == 6)`
3. `(1 * 2) > 5` -> `1 * 2 > 5`
4. `[Merry] > [Happy]` -> `["Merry"] > ["Happy"]`
5. `[1, 2, 3] ++ "look at me!"` -> `"1, 2, 3, " ++ "look at me!"`

For "conditionals", Haskell does not have if "statements", it has if
"expressions", meaning it is a construct available that evaluates the same as
any other expression. There are three parts, the codition, and two expressions
for `then` and `else`. The two expressions must have the same type, and the
condition must be an expression that evaluates to a `Bool`.

Tuples are a type that allow us to store multiple values in a single value. They
look the same on the type and term level, and are defined with parentheses and
commas. Like so: `(Bool, Integer)` and `(True, 5)` (type and term respectively).

Tuples are identified by how many elements they have in them. A "pair" is a
two-element tuple, where a "triple" is a three-element tuple. This number is
also known as the tuple's "arity".

Lists are another way to store multiple values in a single value, but differ
from tuples in some ways. For example, values in the list must all be of the
same type, lists also have their own syntax using square brackets `[]`. Lastly,
the number of items in a list isn't specified by the type, making them more
flexible in regards to how many values are held in them.

# Chapter Exercises

1. `length :: [a] -> Integer`
2. Results of each expression:
  a. `5`
  b. `3`
  c. `2`
  d. `5`
3. `6 / length [1, 2, 3]` returns an error because it returns an `Int`
   specifically, which does not implement `Fractional` the way `(/)` needs.
4. `6 \`div\` length [1, 2, 3]`
5. `2 + 3 == 5` has type `Bool`. Expecting evaluation down to `True`
6. `False :: Bool`
7. Which works/doesn't work, why, and what are resulting values?
  a. Works, because `length` returns an `Int` which implements `Num`, and `2` is a `Num`.
     Evaluates to `True`.
  b. Doesn't work, values in list do not have matching types.
  c. Works, `length` returns `Int`, which implements `Num` and thus `+`, result is `5`
  d. Works, both expressions on either side of `&&` evaluate to `Bool`s, result is `False`.
  e. Doesn't work, `9` is not `Bool`
8. 
  ```
  isPalindrome :: String -> Bool
  isPalindrome s = s == reverse s
  ```
9.
  ```
  myAbs :: Integer -> Integer
  myAbs x = if x < 0 then (-x) else x
  ```
10.
  ```
  f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  f x y = ((snd x, snd y), (fst x, fst y))
  ```

## Correcting Syntax

`ExCorrectingSyntax.hs`

## Match functions to their types

1. c. `Show a => a -> String`
2. b. `Eq a => a -> a -> Bool`
3. a. `(a, b) -> a`
4. d. `(+) :: Num a => a -> a -> a`
