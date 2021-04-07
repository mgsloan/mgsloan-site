---
title: Polymorphic Type-Directed Function Application
date: 2021-04-06
break:
synopsis: GHC plugin to choose a "best-match" argument.
run-in:
primary-image: https://mgsloan.com/images/haskell-placeholder-banner.jpg
category: software
---

In ["Unordered Function Application In Haskell"][], I described an
approach to implementing unordered function application in Haskell.
This approach only works for monomorphic functions and
arguments. Happily, I found a solution to this issue! The goal of this
post is just to sketch the solution, not describe it in detail. For
more detail, see [the code][].

["Unordered Function Application In Haskell"]: /posts/unordered-apply
[the code]: https://github.com/mgsloan/apply-unordered/blob/master/apply-unordered/src/Control/Apply/Unordered/Plugin.h

## The solution: GHC plugin & -XOverlappingInstances logic

Matching arguments to parameters becomes quite ambiguous, so some
concept of a "best fit" for an argument needs to be defined.  As far
as I know, there is no way for type families to work on types
generically, and so a general purpose "best-fit" type family cannot be
defined.

The solution I ended up with is to write a GHC plugin which
substitutes uses of a type family with a [`Nat`][] indicating which
parameter is the "best fit" for the argument.

[`Nat`]: https://hackage.haskell.org/package/base/docs/GHC-TypeNats.html#t:Nat

The idea is to use the same logic as [`-XOverlappingInstances`][] when
determining which parameter is the "best fit" for the argument. In
essence, `-XOverlappingInstances` will select the instance where more
of the type's structure matches. The [code in the plugin][] for
determining the most specific parameter is directly copy-modified from
the [code in GHC][] for selecting an overlapping instance.

[`-XOverlappingInstances`]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#instance-overlap
[code in the plugin]: https://github.com/mgsloan/apply-unordered/blob/111b90ed898648deee97e776752f010ee56877e7/apply-unordered/src/Control/Apply/Unordered/Plugin.hs#L75
[code in GHC]: https://github.com/ghc/ghc/blob/be3c0d62c73361b8805a51a88770991c3b6f9331/compiler/GHC/Core/InstEnv.hs#L956

Here's what it looks like to use this package, via running `stack GHCi
apply-unordered` in the [apply-unordered repository][]:

[apply-unordered repository]: https://github.com/mgsloan/apply-unordered/

```haskell
> :set -fplugin=Control.Apply.Unordered.Plugin
> :m + Data.List
> :t intersperse
intersperse :: a -> [a] -> [a]
> intersperse ? "hello" ? ' '
"h e l l o"
```

Whoah! The arguments are provided in swapped order to a polymorphic
function, and it worked! For `intersperse ? "hello"`, it decided to
provide "hello" as the second argument, because `[a]` is the more
specific match for `[Char]` (`String`). Digging into the type of
`intersperse ? "hello"`:

```haskell
> :t intersperse ? "hello"
intersperse ? "hello"
  :: ApplyAtResult
       (Data.Type.Nat.FromGHC
          (BestParamIxImpl [Char] (Char -> [Char] -> [Char])))
       [Char]
       (Char -> [Char] -> [Char])
> intersperse ? "hello"

<interactive>:13:1: error:
    • No instance for (Show (Char -> [Char]))
    ...
```

As expected, the normalized type is `Char -> [Char]`. The unnormalized
type is more more complicated, and involves an `ApplyAtResult` type
family and a `BestParamIxImpl` type family. The GHC plugin magically
replaces `BestParamIxImpl` type family with a type-level integer
indicating the parameter index the argument should be applied
to.

This approach, a plugin which replaces the type family, was based on
an approach copy-modified from Sandy Maguire's [magic-tyfams
package]().

[magic-tyfams package]: https://hackage.haskell.org/package/magic-tyfams

[`ApplyAt`][] type-class machinery is then used to generate the code which
plumbs the argument to the parameter. This is similar to
[`ApplyByType`][], but directed by a `Nat` rather than checking for
matching types.  Here is how this machinery is defined:

[`ApplyAt`]: https://github.com/mgsloan/apply-unordered/blob/master/apply-unordered/src/Control/Apply/Positional.hs
[`ApplyByType`]: https://github.com/mgsloan/apply-unordered/blob/111b90ed898648deee97e776752f010ee56877e7/apply-unordered-mono/src/Control/Apply/Unordered/Mono.hs#L134

```haskell
-- | Typeclass used to implement 'applyN'. The first type argument is
-- the number of arguments to skip before doing the application.
class ApplyAt (n :: Nat) a f where
  type ApplyAtResult n a f
  applyAtImpl
    :: Proxy n
    -> f
    -> a
    -> ApplyAtResult n a f

instance a ~ b => ApplyAt Z a (b -> r) where
  type ApplyAtResult Z a (b -> r) = r
  applyAtImpl _ f x = f x

instance ApplyAt n a r
      => ApplyAt (S n) a (b -> r) where
  type ApplyAtResult (S n) a (b -> r) = b -> ApplyAtResult n a r
  applyAtImpl _ f y = \x -> applyAtImpl (Proxy :: Proxy n) (f x) y
```
