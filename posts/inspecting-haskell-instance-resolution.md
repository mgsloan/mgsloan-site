---
title: Inspecting Haskell Instance Resolution
date: 2019-09-28
break: Haskell Instance
synopsis: A sketch of how Haskell instance resolution errors could be improved
run-in:
---

GHC's instance resolution errors can often be quite puzzling.  For
example, if I attempt to evaluate `mempty :: Maybe Int` in GHCi
version 8.6.4, it reports:

```
• No instance for (Semigroup Int) arising from a use of ‘mempty’
```

It's easy to imagine that the programmer might be very puzzled by this
error - they might ask "Where in the world did Semigroup come from??".
They'd be quite justified in their perplexity, considering that the
error message is naming something that isn't in their code.

GHC's instance resolution is doing something like this:

1. It attempts to find an instance for `Monoid (Maybe Int)`.

2. This gets matched with `instance Semigroup a => Monoid (Maybe a)`.

3. By unifying `Maybe Int` with `Maybe a`, it deduces that `a ~ Int`.

4. It attempts to find an instance for the superclass, `Semigroup a`.

5. Since `a ~ Int`, it looks for `Semigroup Int`, but doesn't find it.

The problem is that the compiler just reports the error it encounters
in step 5, without providing any context for how it got there. To
understand this error, you need to be able to run typeclass machinery
in your head and have familiarity with the libraries involved. I think
it would be great to add this contextual information to GHC, such that
this variety of errors becomes more comprehensible.

## Sketch of a solution: instance resolution traces

A better error message might look something like:

```
• No instance for (Monoid (Maybe Int)) arising from a use of ‘mempty’
• Due to no instance for (Semigroup Int) arising from a superclass constraint:
    Semigroup a => Monoid (Maybe a)
  with
    a ~ Int
```

I think this is much clearer!  It starts out by talking about a
constraint that comes directly from `mempty`, and then goes on to
describe the constraint that couldn't be resolved, and shows where it
came from.

Let's consider a slightly more complicated example, where I evaluate
`mempty :: Maybe (Product Bool)`, where [`Product`][] comes from
[`Data.Monoid`][].  For this, GHC 8.6.4 reports:

```
• No instance for (Num Bool) arising from a use of ‘mempty’
```

Instead, I think it would be better to report something like this:

```
• No instance for (Monoid (Maybe (Product Bool))) arising from a use of ‘mempty’
• Due to no instance for (Num Bool) arising from superclass constraints:
    Num a =>
    Semigroup (Product a) =>
    Monoid (Maybe b)
  with
    a ~ Bool
    b ~ Product Bool
```

## Alternative: No "with" clause

The "with" clause above defines how the type variables in the
instances are being instantiated.  This is nice because then the
instance resolution trace looks like a list of instance heads.  It
seems clearer to avoid having type variables entirely, though:

```
• No instance for (Monoid (Maybe (Product Bool))) arising from a use of ‘mempty’
• Due to no instance for (Num Bool) arising from superclass constraints:
    Num Bool =>
    Semigroup (Product Bool) =>
    Monoid (Maybe (Product Bool))
```

This is more compact and clear for this example, but not quite as
informative. Specifically, the user can't tell which parts of the
constraints come from the instance definitions, and which parts are
substituted type variables.

One way to differentiate this might be to use ANSI terminal color
codes to show where substitution has occurred!

## Alternative: Full instance context and head

It's also worth consider having the full instance contexts and head.
This way, other than the `with` clauses, the trace would look just
like the instance declaration before `=>`.

```
• No instance for (Monoid (Maybe (Product Bool))) arising from a use of ‘mempty’
• Due to no instance for (Num Bool) arising from superclass constraints:
    instance Num a => Semigroup (Product a)
      with a ~ Bool
    instance Semigroup b => Monoid (Maybe b)
      with b ~ Product Bool
```

## More than just errors

It might also be quite useful to ask for an explanation when instances
successfully resolve. Imagine if we could ask GHCi to explain such
things! It might look like this:

```haskell
> :explain Ord [Maybe Int]

• Ord [Maybe Int]
  • Ord (Maybe Int)
    • Ord Int
```

Since instances can have multiple superclass constraints, we end up
with something analogous to a call tree rather than a stack trace. In
the sketch above, the nested bullets are instances required by the
parent's constraints.

## Explanations for PrintfType

This could be very useful for understanding clever uses of
typeclasses. For example, the [`Text.Printf`][] module in [`base`][]
provides a polyvariadic `printf` function. It can take a variable
number of arguments at a variety of different types:

```haskell
> printf "%d\n" 1
1
> printf "%d %d\n" 1 2
1 2
> printf "%d %d %s\n" 1 2 "wow!"
1 2 wow!
> :t printf
printf :: PrintfType r => String -> r
```

`:explain` could be used to gain some insight into how this works:

```haskell
> :explain PrintfType (Integer -> IO ())

• PrintfType (Integer -> IO ())
  • PrintfArg Integer
  • PrintfType (IO ())

> :explain PrintfType (Integer -> Integer -> IO ())

• PrintfType (Integer -> Integer -> IO ())
  • PrintfArg Integer
  • PrintfType (Integer -> IO ())
    • PrintfArg Integer
    • PrintfType (IO ())

> :explain PrintfType (Integer -> Integer -> String -> IO ())

• PrintfType (Integer -> Integer -> String -> IO ())
  • PrintfArg Integer
  • PrintfType (Integer -> String -> IO ())
    • PrintfArg Integer
    • PrintfType (String -> IO ())
      • PrintfArg [Char]
        • IsChar Char
      • PrintfType (IO ())
```

## Alternative: constraint entailment tree

This could also be written using the constraint entailment operator,
`=>`, causing the tree to be in the opposite order:

```haskell
> :explain PrintfType (Integer -> Integer -> String -> IO ())

( ( ( PrintfType (IO ())
    , ( IsChar Char
      ) => PrintfArg [Char]
    ) => PrintfType (String -> IO ())
  , PrintfArg Integer
  ) => PrintfType (Integer -> String -> IO ())
, PrintfArg Integer
) => PrintfType (Integer -> Integer -> String -> IO ())
```

This reads a bit strange, but I think it is sensible. I haven't
thought through how this would interact with `QuantifiedConstraints`.

In order to make this a fair comparison with the prior example, I
flipped the constraint order.  Since the instance is actually
`(PrintfArg a, PrintfType r) => PrintfType (a -> r)`, the tree would
more likely look like:

```haskell
> :explain PrintfType (Integer -> Integer -> String -> IO ())

( PrintfArg Integer
, ( PrintfArg Integer
  , ( ( IsChar Char
      ) => PrintfArg [Char]
    ) => PrintfType (String -> IO ())
  ) => PrintfType (Integer -> String -> IO ())
) => PrintfType (Integer -> Integer -> String -> IO ())
```

## An old prototype: explain-instance

I've wanted a solution to this for a *long* time.  5 years ago, I
wrote [a prototype][explain-instance], in the form of a Template
Haskell library which takes a rather wild approach.  It would be much
better to implement it directly in GHC, but at the time I was much
more familiar with Template Haskell, and the perverse cleverness of
the approach has some appeal.

With a set of extensions enabled, the following code can be run,

```haskell
import ExplainInstance

$(explainInstanceError [t| Monoid (Maybe Int) |])
```

resulting in the following output:

```haskell
instance Semigroup a => Monoid (Maybe a)
  with a ~ Int

  ERROR instance Semigroup a
    with a ~ Int
```

The way this works is by generating class and instance declarations
that match the existing declarations.  Even for this small example,
the output of `-ddump-splices` can get [quite
large][error-monoid-maybe-int-ddump].  Here's what the relevant
portion of the generated code looks like:

```haskell
main :: IO ()
main = putStrLn (displayInst (resolveMonoid_ (Proxy :: Proxy (Maybe Int))))

class Semigroup_ a where
  resolveSemigroup :: Proxy a -> Inst

class Semigroup_ a => Monoid_ a where
  resolveMonoid :: Proxy a -> Inst

instance {-# OVERLAPPABLE #-} Typeable a => Semigroup_ a where
  resolveSemigroup _ = Inst
    { instHead = "ERROR instance Semigroup a"
    , instTypes = [("a", typeRep (Proxy :: Proxy a))]
    , instConstraints = []
    }

instance (Semigroup_ a, Typeable a) => Monoid_ (Maybe a) where
  resolveMonoid _ = Inst
    { instHead = "instance Semigroup a => Monoid (Maybe a)"
    , instTypes = Inst [("a", typeRep (Proxy :: Proxy a))]
    , instConstraints = [resolveSemigroup (Proxy :: Proxy a)]
    }
```

So, the idea is to have copies of all the instances, but with only one
method, which reifies the tree of instance resolution as a value.

## More elaborate example of explain-instance

Once again with a bunch of extensions, the following code can be run,

```haskell
import ExplainInstance
import Text.Printf

$(explainInstance [t| PrintfType (Int -> Int -> String) |])
```

resulting in the following output:

```haskell
instance (PrintfArg a, PrintfType r) => PrintfType (a -> r)
  with a ~ Int
       r ~ (Int -> [Char])

  instance PrintfArg Int

  instance (PrintfArg a, PrintfType r) => PrintfType (a -> r)
    with a ~ Int
         r ~ [Char]

    instance PrintfArg Int

    instance IsChar c => PrintfType ([c])
      with c ~ Char

      instance IsChar Char
```

See [the repository][github examples] for more examples.

## Where to from here?

My purpose in writing this post is to dust off a very old back-burner
project that I think has merit, in the hopes that it might be
inspiring or perhaps interesting. So, as much as it appeals to me to
champion this idea and see it to fruition, I don't see that being
practical for me to do any time soon.

So, if you find these ideas or some variant inspiring, please do run
with it and make it happen! A natural next step might be to write up a
GHC proposal or GHC ticket about this, seeking some consensus about
the details.

[`Data.Monoid`]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html
[`Product`]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Product
[`Text.Printf`]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Printf.html
[`base`]: http://hackage.haskell.org/package/base
[error-monoid-maybe-int-ddump]: https://gist.github.com/mgsloan/401103e984fc70633cb345c0569fa982
[explain-instance]: http://github.com/mgsloan/explain-instance
[github examples]: https://github.com/mgsloan/explain-instance/tree/master/examples
