---
title: Custom Type Errors for Unordered Function Application
date: 2021-04-06
break:
synopsis: Better type errors via GHC's custom type errors.
run-in:
primary-image: https://mgsloan.com/images/haskell-placeholder-banner.jpg
category: software
---

In the [previous post][], a combination of a typeclass, closed type
family, and associated type family was used to define an operator,
`(?)`. This operator provides type-directed function application,
where the argument is provided to the first parameter that has a
matching type.

This post will describe improving some of the type errors that can
occur when using this operator, by using GHC's [custom type errors][]
feature.

[previous post]: /posts/unordered-apply
[custom type errors]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_errors.html

## Argument mismatch error

With the definitions in the [previous post][], usage of `(?)` with an
argument that has no matching parameter results in unclear type
errors:

```haskell
> cons ? True

<interactive>:1:1: error:
    • No instance for (ApplyByType 'NoArgToMatch Bool Text)
        arising from a use of ‘?’
    • In the expression: cons ? True
      In an equation for ‘it’: it = cons ? True
```

In order for the programmer to understand this error, they would need
to know exactly how the type-level machinery around `ApplyByType` is
structured, and then simulate this machinery in their head. It would
be nice if GHC was more informative for cases like this.  ["Inspecting
Haskell Instance Resolution"][] suggests some ways this could be
improved. Instead the error might be:

```haskell
<interactive>:1:1: error:
    • No instance for (ApplyByType matches3 Bool (Char -> Text -> Text))
        arising from a use of ‘?’
    • Due to no instance for (ApplyByType matches3 Int Text)
      arising from superclass constraints:
        ApplyByType matches1 Bool Text =>
        ApplyByType matches2 Bool (Text -> Text) =>
        ApplyByType matches3 Bool (Char -> Text -> Text)
      with
        matches1 ~ MatchFirstArg Bool Text ~ 'NoArgToMatch
        matches2 ~ MatchFirstArg Bool (Text -> Text) ~ Doesn'tMatch
        matches3 ~ MatchFirstArg Bool (Char -> Text -> Text) ~ Doesn'tMatch
    • In the expression: cons ? True
      In an equation for ‘it’: it = cons ? True
```

This makes it clear that the typeclass machinery recursed through the
function type and determined that none of the arguments matched.

Even just reporting that the instance directly used by `(?)` is
`(ApplyByType Doesn'tMatch Bool (Char -> Text -> Text)` helps clarify
the issue.

["Inspecting Haskell Instance Resolution"]: /posts/inspecting-haskell-instance-resolution/

## Better errors via custom type errors

Such inspection of type-level machinery does not currently exist, so
the only viable approach to improving this error is through use of
GHC's [custom type errors][].

Here's a first try at doing this:

```haskell
instance TypeError (NoMatchForResultError a r)
      => ApplyByType 'NoArgToMatch a r where
  type ApplyByTypeResult 'NoArgToMatch a r =
    TypeError (NoMatchForResultError a r)
  applyByTypeImpl = error "impossible"

type NoMatchForResultError a r =
  'Text "Parameter type " ':$$:
  'Text "  " ':<>: 'ShowType a ':$$:
  'Text "does not occur in the arguments of the function that returns " ':$$:
  'Text "  " ':<>: 'ShowType r ':$$:
  'Text "and so cannot be applied via type directed application.
```

This instance for the `NoArgToMatch` case has a `TypeError` in its
superclass constraint. Now, rather than reporting a missing instance
for this case, the custom type error message will be shown. This
message is defined by `NoMatchForResultError`.

Here's how the various constructors for the error message work:

* `Text` will output the provided `Symbol` (type level string).

* `ShowType` will render the type as a string in the output message.

* `(:<>:)` horizontally concatenates messages.

* `(:$$:)` vertically concatenates messages.

So, with this definition, the above type error becomes:

```haskell
> cons ? True

<interactive>:1:1: error:
    • Parameter type
        Bool
      does not occur in the arguments of the function that returns
        Text
      and so cannot be applied via type directed application.
    • When checking the inferred type
        it :: Char -> Text -> (TypeError ...)
```

Better! This version of the code is [didactic/V4.hs on
github](https://github.com/mgsloan/apply-unordered/blob/master/didactic/V4.hs).

## Reporting the full function type

Above, the type error above only reports the last return value of the
function. It would be clearer to show the full function type, so that
the programmer can directly see that none of the parameters have a
type which matches the argument.

One way to approach this is to have a closed type family which yields
the error when it finds no matching argument:

```haskell
type family HasAMatch a f f0 :: Constraint where
  HasAMatch a (a -> r) f0 = ()
  HasAMatch a (b -> r) f0 = HasAMatch a r f0
  HasAMatch a _ f0 = TypeError (NoMatchErrorMsg a f0)

type NoMatchErrorMsg a f =
  'Text "Parameter type " ':$$:
  'Text "  " ':<>: 'ShowType a ':$$:
  'Text "does not occur in the arguments of the function type " ':$$:
  'Text "  " ':<>: 'ShowType f ':$$:
  'Text "and so cannot be applied via type directed application."

(?)
  :: forall matches a f.
     ( HasAMatch a f f
     , matches ~ MatchFirstArg a f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
(?) = applyByTypeImpl (Proxy :: Proxy matches)
```

Similarly to the `ApplyByType` typeclass, the first argument is the
argument to match, and the second argument is the function type being
recursed on. The added clarity in the error message comes from
passing down the third argument, which has the full function type.

Now the type error is much nicer:


```haskell
> cons ? (1 :: Int)

<interactive>:1:1: error:
    • Parameter type
        Bool
      does not occur in the arguments of the function type
        Char -> Text -> Text
      and so cannot be applied via type directed application.
    • In the expression: cons ? (1 :: Int)
      In an equation for ‘it’: it = cons ? (1 :: Int)
```

This version of the code is [didactic/V5.hs on
github](https://github.com/mgsloan/apply-unordered/blob/master/didactic/V5.hs). Note
that with this approach, the `instance ApplyByType 'NoArgToMatch a r`
is no longer necessary because we handle that case using the
`HasAMatch` [closed type family][].

That's it for now, thanks for reading!  The next post in this series
will use `?` to implement a `reorderArgs` function which will permute
function arguments to match a desired type signature.

<!-- TODO: make "next post" into a link -->

[closed type family]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html#closed-type-families
