---
title: Type-Directed Permutation of Function Parameters
date: 2021-07-07
break:
synopsis: Variadic functions + unordered application.
run-in:
primary-image: https://mgsloan.com/images/haskell-placeholder-banner.jpg
category: software
---

The first post in this series, ["Unordered Function Application In
Haskell"][first post] began with the puzzle of how a
`reorderArgs` function might be implemented, such that the following
works:


```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Control.Apply.Unordered.Mono (reorderArgs)

-- Each example in this group is equal to "hello"
--
-- T.cons :: Char -> T.Text -> T.Text

ex1 = T.cons 'h' ello
ex2 = reorderArgs T.cons 'h' ello
ex3 = reorderArgs T.cons ello 'h'

-- Each example in this group is equal to "hhhello".
--
-- T.justifyRight :: Int -> Char -> T.Text -> T.Text

ex4 = T.justifyRight seven 'h' ello
ex5 = reorderArgs T.justifyRight ello 'h' seven
ex6 = reorderArgs T.justifyRight 'h' ello seven

ello :: T.Text
ello = "ello"

seven :: Int
seven = 7
```

This post, the last in the series, will answer this puzzle, and build
upon [the `(?)` operator][first post], which provides type-directed
function application.

[first post]: /posts/unordered-apply/

## Example of a polyvariadic function: printf

In `ex1` above, `reorderArgs` is taking three arguments, whereas in
`ex4` it is taking four arguments. So, we clearly need a way to define
a polymorphic function which takes any number of
arguments. [`Text.Printf.printf`][] in base is a great example of such
a function.

[`Text.Printf.printf`]: https://hackage.haskell.org/package/base/docs/Text-Printf.html#v:printf

The type of `printf` is `PrintfType r => String -> r`. The `String`
argument provides a C-style format string. The `r` type can be any
function that returns a `String` or `IO ()`, and takes arguments with
types like `String`, `Int`, etc. Some examples of usage:

```haskell
> printf "%d\n" 1
1
> printf "%d %d\n" 1 2
1 2
> printf "%d %d %s\n" 1 2 "wow!"
1 2 wow!
```

The way this works is quite similar to the way the `ApplyByType`
machinery works in prior posts, by recursing on deconstruction of the
function type. However, here the function type is only used in the
output of the method, and so it is using polymorphism in the return
type.

Here are the relevant definitions for `PrintfType`:

```
type UPrintf = (ModifierParser, FieldFormatter)

class PrintfType t where
    spr :: String -> [UPrintf] -> t

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \ a -> spr fmts
                             ((parseFormat a, formatArg a) : args)

instance (IsChar c) => PrintfType [c] where
    spr fmts args = map fromChar (uprintf fmts (reverse args))
```

The details here don't really matter, the things to pay attention to
are:

* The `PrintfType` instance for `a -> r` returns a function, which
  allows it to capture a value for the input.

* The `PrintfType` instance for `String` runs the formatting function
  on the accumulated arguments.

* The arguments are constrained to have a `PrintfArg` instance, so
  that their values can be formatted.

In a prior posting, ["Inspecting Haskell Instance Resolution"][], this
was used as [an example for superclass constraint
explanations][]. Specifically, that post imagines an `:explain`
command in GHCi which would show all instances involved in satisfying
a constraint:

["Inspecting Haskell Instance Resolution"]: https://mgsloan.com/posts/inspecting-haskell-instance-resolution/
[an example for superclass constraint explanation]: https://mgsloan.com/posts/inspecting-haskell-instance-resolution/#explanations-for-printftype

```haskell
> :explain PrintfType (Integer -> String)

• PrintfType (Integer -> String)
  • PrintfArg Integer
  • PrintfType [Char]
    • IsChar Char

> :explain PrintfType (Integer -> Integer -> String)

• PrintfType (Integer -> Integer -> String)
  • PrintfArg Integer
  • PrintfType (Integer -> String)
    • PrintfArg Integer
    • PrintfType [Char]
      • IsChar Char

> :explain PrintfType (Integer -> Integer -> String -> String)

• PrintfType (Integer -> Integer -> String -> String)
  • PrintfArg Integer
  • PrintfType (Integer -> String -> String)
    • PrintfArg Integer
    • PrintfType (String -> String)
      • PrintfArg [Char]
        • IsChar Char
      • PrintfType [Char]
        • IsChar Char
```

For a more elaborate treatment of polyvariadic functions, see [oleg's
page on polyvariadic functions][].

[oleg's page on polyvariadic functions]: http://okmij.org/ftp/Haskell/polyvariadic.html

## The polyvariadic machinery for `reorderArgs`

Similarly to the approach in the [first post][], we need to use a
closed type family to select the instance. The only cases we
need distinguish are function types from everything else, so it looks
like this:

```haskell
-- | A data-kind for 'HasArg' to return.
data HasArgResult
  = ArgPresent    -- ^ Indicates that the type is a function type.
  | NoArg         -- ^ Indicates that the type is not a function type.

-- | Checks whether the specified type is a function type ( @ -> @ ).
type family HasArg f :: HasArgResult where
  HasArg (_ -> _) = 'ArgPresent
  HasArg _ = 'NoArg
```

With this, the typeclass and base case can be written:

```haskell
-- | Typeclass used to implement 'reorderArgs'. The first type
-- argument is used to select instances, and should always be @HasArg
-- f@.
class ReorderArgs (fHasArg :: HasArgResult) f g where
  reorderArgsImpl :: Proxy fHasArg -> f -> g

instance r1 ~ r2 => ReorderArgs 'NoArg r1 r2 where
  reorderArgsImpl _ x = x
```

This instance handles the case where all the function arguments have
been handled, and so all that's left to do is return the result. This
also means that `reorderArgs` can handle 0-argument functions, aka
values. So, `reorderArgs (5 :: Int)` returns `5`.

Quite a bit more machinery is needed to handle functions:

```haskell
instance
    ( HasAMatch a (b -> y) (b -> y)
    , matches ~ MatchFirstArg a (b -> y)
    , result ~ ApplyByTypeResult matches a (b -> y)
    , ApplyByType matches a (b -> y)
    , ReorderArgs (HasArg result) result x
    ) => ReorderArgs 'ArgPresent (b -> y) (a -> x) where
  reorderArgsImpl _ f x =
    reorderArgsImpl (Proxy :: Proxy (HasArg result)) $
    f ? x
```

Let's unpack this:

* The instance head is `ReorderArgs 'ArgPresent (b -> y) (a -> x)`,
  where `b -> y` is the input function, and `a -> x` is the result
  function.

* `HasAMatch a (b -> y) (b -> y)` is a constraint that improves type
  errors.  If `a` does not match any arguments of the function `b ->
  y` (recursing into `y`), then it will cause a type error which
  includes the full function type.  More on this in the [post about
  custom type errors](/posts/unordered-apply-type-errors).

* `matches ~ MatchFirstArg a (b -> y)` checks whether `a` matches `b`,
  and binds that result to the type variable `matches`.

* `result ~ ApplyByTypeResult matches a (b -> y)` uses the type family
  machinery from the [first post][] to determine the type of the
  function after a type-directed partial application to `a`.

* `ApplyByType matches a (b -> y)` is a typeclass constraint which
  allows the use of the `?` operator.

* `ReorderArgs (HasArg result) result x` is the constraint which
  allows us to use `reorderArgsImpl` recursively, to make this
  polyvariadic.

  - `result` is the type of the of the rest of the provided function,
    after it has already been partially applied to `a`.

  - `x` is the type of the rest of the function we are producing.

## Using `reorderArgs`

A cleaner interface is provided by a `reorderArgs` function, which
just supplies a `Proxy` of the appropriate type:

```
reorderArgs :: forall f g. ReorderArgs (HasArg f) f g => f -> g
reorderArgs = reorderArgsImpl (Proxy :: Proxy (HasArg f))
```

Now we can revisit the puzzle from the first post! You can try this
yourself using the code at [didactic/V6.hs on
github](https://github.com/mgsloan/apply-unordered/blob/master/didactic/V6.hs).

```
$ stack ghci V6.hs

> import Data.Text

> ello = Data.String.fromString "ello" :: Text

> :t cons
cons :: Char -> Text -> Text

> reorderArgs cons ello 'h'
"hello"
```

Woohoo!  `reorderArgs` has figured out that it needs to `flip` the
arguments of `cons`!

Naturally, this also works with more arguments, continuing the same session:

```
> :t justifyRight
justifyRight :: Int -> Char -> Text -> Text

> seven = 7 :: Int

> reorderArgs justifyRight ello 'h' seven
"hhhello"

> reorderArgs justifyRight 'he' ello seven
"hhhello"
```

## Machinery explanation

Quite a bit of type-level machinery is making this happen. It might be
helpful to imagine the output of a hypothetical `:explain` command as
described in ["Inspecting Haskell Instance Resolution"][]:

```
> :explain ReorderArgs 'ArgPresent (Char -> Text -> Text) (Text -> Char -> Text)

• ReorderArgs 'ArgPresent (Char -> Text -> Text) (Text -> Char -> Text)
  instance ReorderArgs 'ArgPresent (b -> y) (a -> x)
  with:
    • b ~ Char
    • y ~ Text -> Text
    • a ~ Text
    • x ~ Char -> Text
    • matches ~ MatchFirstArg a (b -> y)
              ~ MatchFirstArg Text (Char -> Text -> Text)
              ~ 'Doesn'tMatch
    • result ~ ApplyByTypeResult matches a (b -> y)
             ~ ApplyByTypeResult 'Doesn'tMatch Text (Char -> Text -> Text)
             ~ Char -> Text
  constraints:

    • ApplyByType matches a (b -> y)
      ~ ApplyByType 'Doesn'tMatch Text (Char -> Text -> Text)
      instance ApplyByType 'Doesn'tMatch a (b -> r)
      with:
        • a ~ Text
        • b ~ Char
        • r ~ Text -> Text
      constraints:

        • ApplyByType (MatchFirstArg a r) a r
          ~ ApplyByType (MatchFirstArg Text (Text -> Text)) Text (Text -> Text)
          ~ ApplyByType 'Matches Text (Text -> Text)
        instance ApplyByType 'Matches a (a -> r)
        with:
          • a ~ Text
          • r ~ Text

    • ReorderArgs (HasArg result) result x
      ~ ReorderArgs (HasArg (Char -> Text)) (Char -> Text) (Char -> Text)
      ~ ReorderArgs 'ArgPresent (Char -> Text) (Char -> Text)
      instance ReorderArgs 'ArgPresent (b -> y) (a -> x)
      with:
        • b ~ Char
        • y ~ Text
        • a ~ Char
        • x ~ Text
        • matches ~ MatchFirstArg a (b -> y)
                  ~ MatchFirstArg Char (Char -> Text)
                  ~ 'Matches
        • result ~ ApplyByTypeResult matches a (b -> y)
                 ~ ApplyByTypeResult 'Matches Char (Char -> Text)
                 ~ Text
      constraints:

        • ApplyByType matches a (b -> y)
          ~ ApplyByType 'Matches Char (Char -> Text)
          instance ApplyByType 'Matches a (a -> r)
          with:
            • a ~ Char
            • r ~ Text

        • ReorderArgs (HasArg result) result x
          ~ ReorderArgs (HasArg Text) Text Text
          ~ ReorderArgs 'NoArg Text Text
          instance ReorderArgs 'NoArg r1 r2
          with:
            • r1 ~ Text
            • r2 ~ Text
```

Whew, quite a lot going on there!  I hope you gleaned something
informative from this more systematic explanation of the machinery.
