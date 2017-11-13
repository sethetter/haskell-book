# Chapter 6: Typeclasses

Typeclasses in Haskell define a set of functions and values that a type can
support which allows that type to function as a member of that typeclass. This
achieves "ad hoc polymorphism" or "constrained polymorphism" as they will refer
to it in the book.

Among the baked in typeclasses in Haskell there is a sort of hierarchy. For
example, `Eq` -> `Ord` -> `Enum`. In order for a type to be enumerated, they
must be able to be put in an order. To be capable of putting it in an order, it
must be able to be checked for equality. This is just a small piece of the
overall hierarchy though.

A typeclass, when defined, specifies what functions and values that a type must
implement. The implementation for different types will vary based on that
specific type, but the type signatures must match what is defined in the
typeclass.

**Note**: in type signatures, type variables are set by the leftmost occurrence
when that function used, and can't be changed once set.

Some typeclasses can be **derived** for types. These include `Eq`, `Ord`,
`Enum`, `Bounded`, `Read`, and `Show`. Deriving means there's no need to
implement your own version of the typeclass. There are some constraints on the
ability to derive in certain circumstances though.

When writing typeclass instances, if we don't cover all cases of the typeclass,
the compiler **will not** complain about it! Instead, an exception will be
thrown at runtime when an undefined instance is used. Adding the `-Wall`
compiler flag to your file will create a warning though.

## Exercises: Eq Instances

Completed in `EqInstances.hs`.

## The `Num` typeclass

The `Num` typeclass is a superclass to all (or at least most) numeric types in
Haskell. These include `Integer`, `Int`, `Float`, `Double`. There are also other
numeric typeclasses to futher create groups of concrete numeric types with
similar characteristics. The main two are `Integral` (whole numbers), and
`Fractional` (not whole numbers).

## Exercises: Tuple Experiment

Both `quotRem` and `divMod` have a type signature of..

```
Integral a => a -> a -> (a, a)
```

Based on this, we can assume we're given back a tuple of either the quotient and
remainder, or of the division and modulus of the two `Integral` inputs..

## Type Defaults

Typeclasses will often implement "defaults" for types that are ambiguously used
in expressions. `Num` will default to `Integer`, `Fractional` will default to
`Double`. Others are listed on The Haskell Report.

This means that `Num a => a -> a` becomes `Integer -> Integer` if a concrete
type is not specified in the expression for that type signature.

It is possible to create more specific (monomorphic) versions of functions that
are otherwise polymorphic.

```
let addIntegers = (+) :: Integer -> Integer -> Integer
```

Behavior stay the same, but a more concrete type signature is put in place.

## Ord

..

## Exercises: Will They Work?

1. Yes, because `Int` implements `Ord`. Result: `5`
2. Yes, because there are types that intersect between `Num` and `Ord`. Result: `LT`
3. No, because the arguments must be a matching type for `compare`.
4. Yes, because there are types that intersect between `Num` and `Ord`. Result: `False`

## Enum

The `Enum` typeclass is for any type that can be enumerated through in some
specific order. The minimal definable instance of `Enum` would define `toEnum`
and `fromEnum`. These are the functions to convert from an `Int` to a specific
item in the enumeration, starting at 0.

## Show

The sole pupose of the `Show` typeclass is for human readability. It is used to
be able to output the values of a type into a human-readable `String`. It is
*not* for serialization under any circumstances. Many base types implement
`Show` already.

## Printing, and side effects

.. notes about `IO` ..

## Read

