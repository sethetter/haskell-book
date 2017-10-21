# Chapter 2 -- Hello, Haskell

**Parameters** are the identifiers in a function expression that represent the
values that may be passed in as arguments. **Arguments** are the values we pass
in to a function for it's parameters.

**Weak head normal form**, or WHNF, is a not-fully-reduced form of an
expression. We reduce down values given as arguments to functions by
substituting them in the body of the function, but we do not reduce the
resulting expressions.

```
(\xy -> x + y) 2
(\y -> 2 + y) <-- No more arguments to apply, WHNF

Or even..

`(\xy -> x + y) 2 3`
`(\y -> 2 + y) 3`
`2 + 3` <-- stop here, WHNF
```

## Exercises: Comprehension Check

1. Update the following expressions to be usable directly in a repl

`half x = x / 2` -> `let half x = x / 2`
`square x = x * x` -> `let square x = x * x`

2. Write a function with a name that supports the following expressions:

```
3.14 * (5 * 5)
3.14 * (10 * 10)
3.14 * (2 * 2)
3.14 * (4 * 4)
```

```
area r = pi * (r * r)
```

## Infix Operators

An **operator** is a function that can be used in infix style. Infix style is
where a function accepts arguments on either side of it, instead of strictly
following the identifier of the function.

```
triple 5 <-- Prefix style
2 + 5 <-- Infix style, (+ is the function)
```

Some functions can be made to use infix style with a syntax adjustment:

```
(div 10 2) == (10 `div` 2)
```

And conversely, we can make an infix operator into a prefix style function:

```
(10 + 2) == ((+) 10 2)
```

## Exercises: Parentheses and Association

Do the two expression have different results due to parentheses placement?

1. Yes, normally multiplication would happen first.
2. No, the parentheses wrap multiply expressions, which would happen first anyways.
3. Yes, division happens before addition normally.


## Exercises: Heal the Sick

Fix the code! First two go in a repl, third is a source file.

1. `let area x = 3.14 * (x * x)` (or maybe use `pi` for `3.14`)
2. `let double x = x * 2`
3.
  ```
  x = 7
  y = 10
  f = x + y
  ``` 
   
## Modular Arithmetic

When we use the `mod` function in Haskell, we're performing an operation called
`modulus`, which uses modular division. In modular division numbers are "wrapped
around" as they progress.

For example..

```
mod 15 12 == 3
```

We can think of it as counting up to 15, but wrapping back to 0 each time we
reach 12. So we count like `1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 1, 2, 3`.

## Parentheses and the `$` operator

The `$` operator is a way of avoiding parentheses and is pretty common in most Haskell code. It's a very simple operator that denotes function application.

It is an infix operator, accepting a function on the left, and an argument for
that function on the right. The following are equivalent:

```
div 10 (3 + 2)
div 10 $ 3 + 2
```

## Sectioning

Sectioning refers to passing around partially applied expressions of infix
operators. Just like we can partially apply arguments to functions accepting
more than one in a prefix style, the same can be done for infix.

```
(+10) 2 == 10 + 2
(3/) 1 == 3 / 1
```

This is useful when we want to apply this "section" of an infix expression with
an unknown set of other values.

## Let and Where, what's the difference?

The primary difference between `let` and `where` is that `let` is an expression,
and is evaluated as such, whereas `where` declares and binds things to those
values within the scope of the function.

## Exercises: A Head Code

Determine what these functions will return, then confirm in repl. Left is my
guess, right is what the repl returned.

1. `5` -> `5`
2. `25` -> `25`
3. `30` -> `30`
4. `6` -> `6`

*More in practice.hs*

# Chapter Exercises

## Parenthesization

1. `2 + (2 * 3) - 1`
2. `10 ^ (1 + 1)`
3. `((2 ^ 2) * (4 ^ 5)) + 1`

## Equivalent Expressions

1. Equivalent
2. Equivalent
3. Not equivalent, `37` should be second argument to `(-)`
4. Not equivalent, `div` does integer division
5. Not equivalent, parentheses change order of evaluation

## More fun with functions

Translating source code into code for a repl:

```
let z = 7
let y = z + 8
let x = y ^ 2
let waxOn = x * 5
```

Order of declaration matters in repl.

1.
  ```
  10 + waxOn == 1135
  (+10) waxOn == 1135
  (+10) waxOn == 1135
  (-) 15 waxOn == -1110
  (-) waxOn 15 == 1110
  ```

2. *Just entering some code into the repl*
   
3.
  ```
  Guess -> 3375
  Actual -> 3375
  ```
  
4. *Write above `waxOn` as expression with `where` in `practice.hs`*

5. *Added triple function to same source file, get same result for `triple waxOn`*

6. *Add `waxOff` function to same source file. Manipulate to do different things*
