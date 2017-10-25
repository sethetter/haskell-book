# Chapter 3 -- Strings!

This chapter covered some of the basics of working with with the `String` data
type, which is simply a type alias for `[Char]`.

**Type aliases**, or type synonyms, are type names that are just an alternate
identifier for another type. `String`, for example, is simply an alias for
`[Char]`.

We covered two different ways to print a string to the console, `putStr` and
`putStrLn`. The latter includes a newline at the end of the string by default.

Next we learned about top-level versus local definitions. The idea here is that
a top level definition of a a function or variable is available to the rest of
the module, but something defined in a `let` or `where` expression within
another expression is only available to the expression in which it is defined.

## Exercises: Scope

1. Yes. In gchi, as long as a used value is defined before it is used, it's
   available.
2. No, there is no `h` defined in the repl.
3. No, `d` is only available in the scope of the `area` function.
4. Yes, because `d` is available in the nested `where` of the `area` function.

In Haskell there are couple different types of concatenation functions. The ones
we covered in this chapter include `++` and `concat`. The first, `++`, is an
infix operator used like so: `"hello" ++ " " ++ "world"`.

By contrast, `concat` is a normal prefix function. It takes, as it's single
argument, a list of lists to concatenate. Since the `String` data type is just
an alias for `[Char]`, this works for strings as well as lists of other things.

```
concat ["hello", " ", "world"] == "hello world"
concat [[1, 2, 3], [4, 5, 6], [7]] == [1, 2, 3, 4, 5, 6, 7]
```

## Exercises: Syntax Errors

1. `(++) [1, 2, 3] [4, 5, 6]`
2. `"<3" ++ " Haskell"`
3. `concat ["<3", " Haskell"]`

Lastly, the chapter briefly covered more list functions that are available.

- `:` is the "cons" operator, which adds an item to the beginning of a list.
- `head` takes a list and returns the first item.
- `tail` takes a list and returns everything *except* the first item.
- `take` takes an `Int` (`x`) and a list and returns the first `x` number of
  items from the list.
- `drop` is the inverse of `take`, and returns the remaining list after dropping
  `x` number of items.
- The `!!` operator is used to return an item at a specified index of a list,
  like `myList !! 2`.
  
Note that some of the above list functions are considered **unsafe** because
they will cause an exception if used in the wrong context. For example, using
`head` on an empty list, or using `!!` to access and index that is not available.

It's generally bad practice to use these functions as opposed to their safer
alternatives, but apparently we will be using them for now until further along.

# Chapter Exercises

## Reading Syntax

1. Correct the syntax errors.
  a. Correct. `concat [[1, 2, 3], [4, 5, 6]]`
  b. Incorrect. Corrected -> `(++) [1, 2, 3] [4, 5, 6]`
  c. Correct `(++) "hello" " world"`
  d. Incorrect. Corrected -> `"hello" ++ " world"`
  e. Incorrect. Corrected -> `"hello" !! 4`
  f. Correct. -> `(!!) "hello" 4`
  g. Incorrect. Corrected -> `take 4 "lovely"`
  h. Correct. `take 3 "awesome"`
  
2. Match results with their expressions
  a. d
  b. c
  c. e
  d. a
  e. b

## Building Functions

1. Write a function that takes the given input and returns the specified output.
  a. `"Curry is awesome" ++ "!"`
  b. `"Curry is awesome!" !! 4`
  c. `drop 9 "Curry is awesome!"`
  
2. Rewrite the above as reusable functions in a source file ->
   `ExBuildingFunctions.hs`
   
3. Write a function to get the third letter from a string. ->
   `ExBuildingFunctions.hs`
   
4. Write a function to get a letter by input index from a string that doesn't
   change. -> `ExBuildingFunctions.hs`
   
5. Write a function that starts with "Curry is awesome" and results in "awesome
   is Curry", not intended to be general purpose, specific to just this string
   -> `ExBuildingFunctions.hs`
   
6. Put the `rvrs` function from the previous exercise into it's own module, with
   a `main` function that prints the result. -> `Reverse.hs`
