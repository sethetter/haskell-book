# Chapter 1 -- Lambda Calculus

## What I Learned

Haskell is based on **lambda calculus**. Lambda calculus is concerned with the
abstraction of variable values from expressions. A lambda **function** or
**abstraction** is an expression that contains references to variables, and shows
a body that utilizes those values.

The **head** is the part that defines what variables that expression accepts,
and the **body** puts those variables to use.

*Note that I'm using `+` as part of the example below, but in the example I
followed this was not common. I'm using it to make the examples more grounded in
the world of programming.*

```
( \x . x + 1)
   ^   ^
   |   |
 head  |
     body
```

There is also the concept of **free variables**. These are variables that show
up in the body of expressions that are not bound in the head of the function.
This makes it not possible to define their value by applying values to the head
of a function.

```
(\x.x+z) 1 -- Apply the 1 in place of x
(1z) -- Can't apply any further! No function accepting a parmeter of z

(\xz.x+z) 1 2 -- Apply the 1 for x
(\z.1+z) 2 -- Apply the 2 to z
(1+2) -- Reduce!
3 -- No free variables! Means we can reduce all the way down
```

This process shown above of reducing an expression step by step is called **beta
reduction**. In lambda calculus there are often expressions like this:

```
(\xy.xyz)(\a.aa)(\b.b)
```

In these expressions, a lambda is passed around as a variable itself. The
corresponding concept for this is software dev is "first class functions". The
previous expression beta reduces as follows.

```
(\xy.xyz)(\a.aa)(\b.b)
(\y.(\a.aa)yz)(\b.b)
(\a.aa)(\b.b)z
(\b.b)(\b.b)z
(\b.b)z
z
```

When we have reached a point of no longer having lambdas that we need to apply
arguments to, this is called **beta normal form** or just **normal form**.

Next the concept of **combinators** was covered. A combinator is any lambda
expression that *has no free variables*. There wasn't much more discussed
regarding combinators, just they are a special class of lambda function.

Lastly, the concepts of **converging** and **divergence** were discussed. A
**combinator** is simply a lambda expression that can be evaluated all the way
down to a value. Even if this "value" is just a variable. The idea is that there
are no more *lambda expressions* to apply arguments to.

An expression is **divergent** if it can't be beta reduced, or results in some
sort of infinite or circular application pattern. The following example is
called **omega**.

```
(\x.xx)(\x.xx)
```

Reducing this immediately results in the same expression, this make it divergent.

That's it!


## Exercises

Equivalence
1. B
2. C
3. B

Combinators 
1. Y
2. N
3. Y
4. Y
5. N

Normal form or diverge?
1. Normal
2. Diverge
3. Normal

Beta reduce
1. z
2. bb
3. qq
4. yy
5. yy
6. aac
7. (\z1.za)

Number 7 work (leftmost, outermost):
```
(\xyz.xz(yz))(\x.z)(\x.a)
(\yz1.(\x.z)yz1)(\x.a)
(\z1.(\x.z)(\x.a)z1)
(\z1.z(\x.a)z1)
(\z1.za)
```
