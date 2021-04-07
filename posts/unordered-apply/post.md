---
title: Unordered Function Application In Haskell
date: 2021-04-03
break:
synopsis: Type system tricks to implement type-directed function application.
run-in: Here is a little puzzle:
primary-image: https://mgsloan.com/images/haskell-placeholder-banner.jpg
category: software
---

Here is a little puzzle: How might a function like `reorderArgs` be
defined, such that the code below compiles?

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

I found it quite surprising that this is possible to define. One thing
that makes it feasible is a restriction that all arguments are
monomorphic. Solving this puzzle is a decent didactic exercise, as it
involves a variety of type-level techniques:

1. Using [closed type families][] to direct instance selection.

2. Using [custom type errors][] to improve the error messaging.

3. Using typeclasses to implement [polyvariadic functions][].

**1** is described in this post. **2** and **3** will be described in
subsequent posts in the series.

<!-- TODO: Link to the posts -->

The functionality described in these posts is available on Hackage, in
the [apply-unordered-mono package][]. It is also possible to
generalize this to polymorphic functions, via a GHC compiler
plugin. This is demonstrated by the related [apply-unordered
package][].

<!-- TODO: Link to the posts -->

[closed type families]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html#closed-type-families
[custom type errors]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_errors.html
[polyvariadic functions]: http://okmij.org/ftp/Haskell/polyvariadic.html
[apply-unordered-mono package]: http://hackage.haskell.org/package/apply-unordered-mono
[apply-unordered package]: http://hackage.haskell.org/package/apply-unordered

## Type-directed application of a single argument

A step towards a solution to this is to write a function which
implements type-directed application of a single argument.  Lets name
this function `(?)`, used like this:

```haskell
appendEllo :: Char -> Text
appendEllo = T.cons ? ello
```

`(?)` takes any function `f`, and an argument `a` that matches the
type of one of its parameters. It will then partially apply `f` to
this argument. So, the example above is equivalent to:

```haskell
appendEllo :: Char -> Text
appendEllo = (\a arg1 -> T.cons arg1 a) ello
```

`(?)` should also be able to provide a value for the first parameter:

```haskell
prependH :: Text -> Text
prependH = T.cons ? 'h'
```

The plan is to automatically construct this code by using typeclass
machinery!

## A failed attempt

One way to approach this might be a typeclass with an associated type
family for the result:

```haskell
class ApplyByType a f where
  type ApplyByTypeResult a f
  (?)
    :: f
    -> a
    -> ApplyByTypeResult a f
```

The type-level algorithm implemented by these instances should check
if the first parameter of `f` matches `a`. If it does, then apply the
function:

```haskell
instance ApplyByType a (a -> r) where
  type ApplyByTypeResult a (a -> r) = r
  f ? x = f x
```

If it doesn't, then it should recurse down the right-hand-side of the
`(->)` type:

```haskell
instance ApplyByType a r => ApplyByType a (b -> r) where
  type ApplyByTypeResult a (b -> r) = b -> ApplyByTypeResult a r
  f ? y = \x -> f x ? y
```

However, this doesn't work! GHC complains:

```haskell
Conflicting family instance declarations:
  ApplyByTypeResult a (a -> r) = r
  ApplyByTypeResult a (b -> r) = b -> ApplyByTypeResult a r
```

The problem is that open type families do not allow overlap in their
instances.

The code that results in this error is [didactic/V1.hs on
github](https://github.com/mgsloan/apply-unordered/blob/master/didactic/V1.hs).

## Attempted fix: use a closed type family

[Closed type families][] permit such overlap, and so provide a nice
workaround for this problem. Here's what using a closed type family
might look like:

```haskell
type family ApplyByTypeResult a f where
  ApplyByTypeResult a (a -> r) = r
  ApplyByTypeResult a (b -> r) = b -> ApplyByTypeResult a r
```

This can be tried out in GHCi, by using `:k!` to ask GHC to normalize
the type - expanding synonyms and applying type functions:

```haskell
> :k! ApplyByTypeResult Text (Char -> Text -> Text)
ApplyByTypeResult Text (Char -> Text -> Text) :: *
= Char -> Text
```

Awesome!  It's figured out that `Text` matches the second parameter, and
so unordered partial application results in a `Char -> Text` type.

Trying it along with the typeclass:

```haskell
class ApplyByType a f where
  (?) :: f -> a -> ApplyByTypeResult a f

instance ApplyByType a (a -> r) where
  f ? x = f x

instance ApplyByType a r => ApplyByType a (b -> r) where
  f ? y = \x -> f x ? y
```

Argh! Foiled again:

```haskell
• Couldn't match expected type ‘ApplyByTypeResult a (b -> r)’
              with actual type ‘b -> ApplyByTypeResult a r’
• The lambda expression ‘\ x -> f x ? y’
  has one argument,
  but its type ‘ApplyByTypeResult a (b -> r)’ has none
  ...
```

I think the problem here is that GHC has insufficient information to
choose between the different cases of the closed type family. In order
to reject the first equation, it would need to know that `a` does not
unify with `b`.

The code that results in this error is [didactic/V2.hs on
github](https://github.com/mgsloan/apply-unordered/blob/master/didactic/V2.hs).

## Fix: use a closed type family to choose the instance!

A well-known trick among typeclass machinery enthusiasts is to use
a closed type family to choose an instance. Here's how this works:

1. A type parameter is added to the typeclass, which is used to select
   the instance.

2. A `Proxy` parameter is added to the method(s), used to specify the
   type parameter when using the class.

3. A closed type family is used to compute the type to pass in via the
   `Proxy`.

Here's what it looks like for this application:

```haskell
data MatchArgResult
  = Matches
  | Doesn'tMatch
  | NoArgToMatch

type family MatchFirstArg a f :: MatchArgResult where
  MatchFirstArg a (a -> r) = 'Matches
  MatchFirstArg a (b -> r) = 'Doesn'tMatch
  MatchFirstArg _ _ = 'NoArgToMatch
```

The `MatchArgResult` constructors are used at the type-level via the
`DataKinds` extension. This is not really necessary, but it's rather
nice for having a proper type-level datatype to use for the result of
the discriminating closed type family.

Trying it out in GHCi:

```haskell
> :k! MatchFirstArg Char (Char -> Text -> Text)
MatchFirstArg Char (Char -> Text -> Text) :: MatchArgResult
= 'Matches

> :k! MatchFirstArg Text (Char -> Text -> Text)
MatchFirstArg Text (Char -> Text -> Text) :: MatchArgResult
= 'Doesn'tMatch

> :k! MatchFirstArg Char Text
MatchFirstArg Char Text :: MatchArgResult
= 'NoArgToMatch
```

It works! When the argument type matches the first parameter, the
result is `Matches`, and when it does not, the result is
`Doesn'tMatch`. When it isn't a function at all, the result is
`NoArgToMatch`.

This closed type family will allow us to choose between
`ApplyByTypeResult` instances, avoiding any overlap issues.  The
method also gets renamed to `applyByTypeImpl`, as the `(?)` function
will get defined in terms of it:


```haskell
class ApplyByType (matches :: MatchArgResult) a f where
  type ApplyByTypeResult matches a f
  applyByTypeImpl
    :: Proxy matches
    -> f
    -> a
    -> ApplyByTypeResult matches a f

instance ApplyByType 'Matches a (a -> r) where
  type ApplyByTypeResult 'Matches a (a -> r) = r
  applyByTypeImpl _ f x = f x
```

The recursive case gets a little trickier:

```haskell
instance ApplyByType (MatchFirstArg a r) a r
      => ApplyByType 'Doesn'tMatch a (b -> r) where
  type ApplyByTypeResult 'Doesn'tMatch a (b -> r) =
    b -> ApplyByTypeResult (MatchFirstArg a r) a r
  applyByTypeImpl _ f y =
    \x -> applyByTypeImpl (Proxy :: Proxy (MatchFirstArg a r)) (f x) y
```

In 3 different places we need to apply the `MatchFirstArg` type
function in order to discriminate which instance to recurse into. It
might be helpful to reference the old simpler (but non-functioning)
definition:

```haskell
instance ApplyByType a r
      => ApplyByType a (b -> r) where
  type ApplyByTypeResult a (b -> r) =
    b -> ApplyByTypeResult a r
  f ? y =
    \x -> f x ? y
```

Giving this new definition a try:

```haskell
λ applyByTypeImpl (Proxy :: Proxy 'Doesn'tMatch) T.cons ("ello" :: T.Text) $ 'h'
"hello"
```

Woohoo! It figured out that `"ello" :: T.Text` needs to be passed in
as the second argument to `T.cons`.

However, it's a bit inconvenient to need to pass in that `Proxy`. The
convenience of `(?)` can be regained by writing a helper function
which applies `MatchFirstArg`:

```haskell
(?)
  :: forall matches a f.
     ( matches ~ MatchFirstArg a f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
(?) = applyByTypeImpl (Proxy :: Proxy matches)
```

Giving that a try:

```haskell
> T.cons ? T.pack "ello" ? 'h'
"hello"
```

Boom! Unordered, type-directed function application!

This version of the code is [didactic/V3.hs on
github](https://github.com/mgsloan/apply-unordered/blob/master/didactic/V3.hs). The
next post in this series will improve the type errors that occur when
the argument type mismatches with all of the parameters.

<!-- TODO: make "next post" into a link -->

## Alternative approach: functional dependencies

In the [discussion on reddit][], [u/lightandlight][] described a [more
elegant implementation][] using functional dependencies with
overlapping instances:

[discussion on reddit]: https://www.reddit.com/r/haskell/comments/mk11iu/unordered_function_application_in_haskell_type/
[u/lightandlight]: https://www.reddit.com/user/lightandlight/
[more elegant implementation]: https://www.reddit.com/r/haskell/comments/mk11iu/unordered_function_application_in_haskell_type/gtemzu9/

```haskell
class Apply f x y | f x -> y where
  apply :: f -> x -> y

instance {-# overlappable #-} (x ~ (a -> b)) => Apply x a b where
  apply = ($)

instance {-# overlapping #-} Apply b a b' => Apply (a' -> b) a (a' -> b') where
  apply f a = \a' -> apply (f a')
```

Very cool! However, this approach leads to more obscure error
messages. Lets say we supply the wrong argument type:

```haskell
> cons ? True

<interactive>:1:1: error:
    • Couldn't match type ‘Text’ with ‘Bool -> b'’
        arising from a use of ‘?’
    ...
```

A bit of a head-scratcher! If something like ["Inspecting Haskell
Instance Resolution"][] was implemented in GHC, then this error might
instead look like:

```haskell
> cons ? True

<interactive>:11:1: error:
    • Couldn't match type ‘Text’ with ‘Bool -> b'’
        arising from a use of ‘?
    • While resolving the following instances:
        (Text ~ (Bool -> b)) =>
        Apply Text Bool b
      due to
        Apply Text Bool b' =>
        Apply (Text -> Text) Bool (Text -> b')
      due to
        Apply (Text -> Text) Bool (Text b') =>
        Apply (Char -> Text -> Text) Bool (Char -> Text -> b')
    ...
```

This makes it a bit clearer that it recursed on the function type and
errored out on attempting to satisfy the `(x ~ (a -> b))` constraint.

With the implementation here, the error is instead:

```haskell
λ cons ? True

<interactive>:1:1: error:
    • No instance for (ApplyByType 'NoArgToMatch Bool Text)
        arising from a use of ‘?’
    ...
```

Still not great, but I think a bit clearer. An upcoming post will
describe using GHC custom type errors to further improve this
circumstance.

<!-- TODO: link to post -->

["Inspecting Haskell Instance Resolution"]: /posts/inspecting-haskell-instance-resolution/

## Appendix: Why?

You might be wondering why you would ever want to use this.  Honestly,
I can't think of many practical applications, and so the
[apply-unordered-mono package][] is in the [`ACME` category][] on
Hackage.  Even so, I think it's pretty cool that this is possible.

[`ACME` category]: http://hackage.haskell.org/packages/#cat:ACME

One potential use-case might be to handle shifting APIs in
dependencies.  If a library changes the order of its arguments, you
might be able to use the `(?)` operator to be compatible with versions
before and after this change, without using the C preprocessor.

The inspiration for this package was speculation about a programming
language where arguments are discriminated by type instead of
position. To experiment with this, it turned out to be more
straightforward to directly implement the idea in Haskell rather than
writing a whole new language. Here are some reasons why I think this
is an interesting idea for language design:

* Parameter order is often arbitrary, and so requires effort to
memorize.

* It is safer to have distinct types for all arguments, to avoid bugs
due to inadvertent transposition.

Named parameters can help with this, but then you need to memorize the
names! Conventions where the name is the same as the type adds
verbosity / redundancy.

In practice, many newtypes would be involved in usage of type-directed
application.  Newtypes might even be associated with the function
definition, to provide something akin to named parameters.

Ordered arguments do have some nice advantages, though:

* Polymorphism. Matching of arguments with parameters gets ambiguous
  when type variables are involved.

* Ordered arguments provides more direct visual comparison of
  calls of the same function.

To me, this suggests an alternative implementation approach.  Rather
than incorporating type-directed application directly into the
language, or as a library, it can be part of edit-time tooling.  For
example, the reformatting tool might handle reordering the arguments.
