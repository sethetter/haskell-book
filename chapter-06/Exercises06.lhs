Does it typecheck?
----

1. No, we must derive `Show` for the `Person` datatype.

> data Person = Person Bool deriving (Show)
>
> printPerson :: Person -> IO ()
> printPerson person = putStrLn $ show person

2. No, it needs the `Eq` typeclass on the `Mood` data type.

> data Mood = Blah | Woot deriving (Eq, Show)
>
> settleDown x = if x == Woot then Blah else x

3. a. Values of type `Mood`.
3. b. A 'no instance' error will occur for `Num`.
3. c. A 'no instance' error occurs for `Ord`.

4. Yes, it typechecks as is. Though trying to inspect `s1` in ghci
   will result in a no instance for `Show` on `Object -> Sentence`.

> type Subject = String
> type Verb = String
> type Object = String
>
> data Sentence
>   = Sentence Subject Verb Object
>   deriving (Eq, Show)
>
> s1 = Sentence "dogs" "drool"
> s2 = Sentence "Julie" "loves" "dogs"

Given a datatype, what can we do?
----

Given the following datatypes..

> data Rocks = Rocks String deriving (Eq, Show)
> data Yeah = Yeah Bool deriving (Eq, Show)
> data Papu = Papu Rocks Yeah deriving (Eq, Show)

1. Doesn't typecheck, provided values don't use the necessary data constructors.

< phew = Papu "chases" True -- Incorrect
> phew' = Papu (Rocks "chases") (Yeah True)

2. Typechecks.

truth = Papu (Rocks "chomskydoz") (Yeah True)

3. Typechecks, since we derive `Eq` on `Papu`

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

4. Does not typecheck, `Papu` does not implement `Ord`. To derive
   it, we would need to derive it for `Yeah` and `Rocks` as well.

< comparePapus :: Papu -> Papu -> Bool
< comparePapus p p' = p > p'

Match the types
----

Can the second type signatures be used in place of the first?

1. Can't replace first with second. Type at the term level is constrained to `Num`.
> i :: Num a => a -- First
< i :: a -- Second
> i = 1

2. Can't replace, `Num` is not constrained enough for `1.0`, must be `Fractional`.
> f :: Float
< f :: Num a => a
> f = 1.0

3. Can replace! `1.0`'s most general type is `Fractional a => a`
< f :: Float
> f :: Fractional a => a
> f = 1.0

4. Can replace, because `Float` implements `RealFrac`.
< f :: Float
> f :: RealFrac => a
> f = 1.0

5. Can replace, but not sure why you would want to?
< freud :: a -> a
> freud :: Ord a => a -> a
> freud x = x

6. Can replace.
< freud' :: a -> a
> freud' :: Int -> Int
> freud' x = x

7. Can't replace, because, `myX` has a concrete type of `Int`.
> myX = 1 :: Int
>
> sigmund :: Int -> Int
< sigmund :: a -> a
> sigmund x = myX

8. Can't replace, because `myX` is already more specific
> myX' = 1 :: Int
>
> sigmund' :: Int -> Int
< sigmund :: Num a => a -> a
> sigmund' x = myX'

9. Can replace, makes it more specific.
> import Data.List
>
< jung => Ord a => [a] -> a
> jung :: [Int] -> Int
> jung = head . sort

10. Can replace! Makes more general.
> import Data.List
>
< young => [Char] -> Char
> young :: Ord a => [a] -> a
> young = head . sort

11. Can't replace, `mySort` is constrained to the concrete type `Char`.
> mySort :: [Char] -> Char
> mySort = sort
>
> signifier :: [Char] -> Char
< signifier :: Ord a => [a] -> a
> signifier = head . sort


Type-Kwon-Do
----

1.
> chk :: Eq b => (a, b) -> a -> b -> Bool
> chk t _ y = snd t == y

2.
> arith :: Num b => (a, b) -> Integer -> a -> b
> arith t _ _ = snd b + 2
