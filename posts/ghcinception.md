---
title: GHCinception: Running GHCi in GHCi
date: 2018-07-28
break: GHCinception: Running
synopsis: My patches were merged, allowing GHC to be loaded into GHCi and run!
---

I'm happy to announce that you can now easily load GHC into GHCi! I've been
using this for about a month now, and for me it makes GHC development far more
pleasant than using `make`. This can often lead to iteration times of only
**10-30 seconds** to try out some modified behavior.

For me, GHCi usage is crucial to efficient Haskell development. One larger
project I work on takes a whopping 15 minutes to build, whereas loading the same
code in GHCi takes a little bit less than 1 minute.

[GHCi][] is quite a marvelous thing. Despite being an interpreter, it can run
complicated programs quite quickly and correctly - I typically barely notice a
difference in performance. One of the main reasons for this is that it still
uses the compiled code for your dependencies. It also lets you conveniently
query lots of information and try things out in a [REPL][].

[GHCi]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html
[REPL]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

## How to load GHC into GHCi

If you have a recent checkout of GHC, and have built the GHC stage 2 compiler
located at `/inplace/bin/ghc-stage2`, then you can do the following to load GHC
into GHCi:

```
$ ./utils/ghc-in-ghci/run.sh -fobject-code -j8
```

Tweak the `N` in `-jN` as is suitable for your system - it indicates the degree
of module-level build parallelism. The number of logical cores you have is a
reasonable guideline.

This should result in GHCi loading up the code for GHC:

```haskell
mgsloan@treetop:~/oss/haskell/ghc$ ./utils/ghc-in-ghci/run.sh -fobject-code -j8
+ export _GHC_TOP_DIR=./inplace/lib
+ exec ./inplace/bin/ghc-stage2 --interactive -ghci-script ./utils/ghc-in-ghci/settings.ghci -ghci-script ./utils/ghc-in-ghci/load-main.ghci -odir ./ghci-tmp -hidir ./ghci-tmp +RTS -A128m -RTS -fobject-code
GHCi, version 8.7.20180718: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/mgsloan/.ghci
package flags have changed, resetting and loading new packages...
Loaded GHCi configuration from ./utils/ghc-in-ghci/settings.ghci
[  1 of 493] Compiling GhcPrelude       ( compiler/utils/GhcPrelude.hs, ghci-tmp/GhcPrelude.o )
[  2 of 493] Compiling FiniteMap        ( compiler/utils/FiniteMap.hs, ghci-tmp/FiniteMap.o )
[  3 of 493] Compiling FastMutInt       ( compiler/utils/FastMutInt.hs, ghci-tmp/FastMutInt.o )
[  4 of 493] Compiling FastFunctions    ( compiler/utils/FastFunctions.hs, ghci-tmp/FastFunctions.o )
[  5 of 493] Compiling Exception        ( compiler/utils/Exception.hs, ghci-tmp/Exception.o )
[  6 of 493] Compiling EnumSet          ( compiler/utils/EnumSet.hs, ghci-tmp/EnumSet.o )
[  7 of 493] Compiling Encoding         ( compiler/utils/Encoding.hs, ghci-tmp/Encoding.o )
...
```

This does take a while to load, as it's loading 493 modules. On my machine it
clocks in at around 8 minutes. The main reason that this takes so long is that
it needs to do object code generation. I bet it'd also go a lot faster if I
wasn't using an unoptimized `devel2` flavour build of `ghc-stage2`. This flavour
includes potentially expensive assertions.

Once it has finished loading, you can use GHCi to ask for information as usual!
For example, let's take a look at the datatype for a core expression:

```haskell
Ok, 493 modules loaded.
Loaded GHCi configuration from ./utils/ghc-in-ghci/load-main.ghci
λ :m + CoreSyn
λ :info Expr
data Expr b
  = Var Var.Id
  | Lit Literal.Literal
  | App (Expr b) (Arg b)
  | Lam b (Expr b)
  | Let (Bind b) (Expr b)
  | Case (Expr b) b TyCoRep.Type [Alt b]
  | Cast (Expr b) TyCoRep.Coercion
  | Tick (Tickish Var.Id) (Expr b)
  | Type TyCoRep.Type
  | Coercion TyCoRep.Coercion
```

Beautiful! While `-fobject-code` does take quite a while to do an initial load,
subsequent loads can be very fast. For me, with no code changes, it only takes
about **7 seconds**.

## How to run GHCi within GHCi

The ghci script sets up some default arguments:

```
:set args --interactive -ghci-script utils/ghc-in-ghci/inner.ghci
```

Due to these defaults, just running `main` brings up a nested GHCi:

```haskell
λ main
GHCi, version 8.7.20180718: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/mgsloan/.ghci
Loaded GHCi configuration from utils/ghc-in-ghci/inner.ghci
Prelude [inner]> :{
Prelude| putStrLn $ unwords $
Prelude| "Wow, we're evaluating code" : replicate 2 "*inside* GHCi"
Prelude| :}
Wow, we're evaluating code *inside* GHCi *inside* GHCi
Prelude [inner]>
```

There's a different GHCi prompt there because the `inner.ghci` contains

```haskell
:set prompt "%s [inner]> "
```

When using this for development, I found that it was helpful to distinguish the
inner ghci prompt from the outer.

## Modifying, reloading, and trying some new behavior

Lets say I want to [change some type error messages][ghc-proposals #156]. First,
I make a change to the code:

```diff
diff --git a/compiler/typecheck/TcErrors.hs b/compiler/typecheck/TcErrors.hs
index ecb404289a..32d7cffaab 100644
--- a/compiler/typecheck/TcErrors.hs
+++ b/compiler/typecheck/TcErrors.hs
@@ -1894,11 +1894,11 @@ misMatchMsg ct oriented ty1 ty2
   where
     herald1 = conc [ "Couldn't match"
                    , if is_repr     then "representation of" else ""
-                   , if is_oriented then "expected"          else ""
+                   , if is_oriented then "wanted"            else ""
                    , what ]
     herald2 = conc [ "with"
                    , if is_repr     then "that of"           else ""
-                   , if is_oriented then ("actual " ++ what) else "" ]
+                   , if is_oriented then ("found " ++ what)  else "" ]
     padding = length herald1 - length herald2

     is_repr = case ctEqRel ct of { ReprEq -> True; NomEq -> False }
```

Then I do a reload, run the inner GHCi, and try it out:

```haskell
λ :r
[ 22 of 493] Compiling Hooks[boot]      ( compiler/main/Hooks.hs-boot, ghci-tmp/Hooks.o-boot )
... Omitted about 40 boot modules that load really quickly ...
[377 of 493] Compiling TcErrors         ( compiler/typecheck/TcErrors.hs, ghci-tmp/TcErrors.o )
... A few more boot modules ...
Ok, 493 modules loaded.
λ
```

This only took about **8 seconds**!!! Invoking the inner GHCi takes **less than
a second**:

```haskell
λ main
GHCi, version 8.7.20180718: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/mgsloan/.ghci
Loaded GHCi configuration from utils/ghc-in-ghci/inner.ghci
Prelude [inner]> "testing" ++ Nothing

<interactive>:1:14: error:
    • Couldn't match wanted type ‘[Char]’ with found type ‘Maybe a0’
    • In the second argument of ‘(++)’, namely ‘Nothing’
      In the expression: "testing" ++ Nothing
      In an equation for ‘it’: it = "testing" ++ Nothing
Prelude [inner]>
```

For me, this is a game changer for GHC development. In particular, this means
that you can make a change and very rapidly try out the change in behavior. On
my machine, it often takes **less than 10 seconds to make a change and try it
out**.  This is drastically faster than `make` - I'm not even going to try, but
I'm pretty sure I'd be waiting around for a few minutes.

[ghc-proposals #156]: https://github.com/ghc-proposals/ghc-proposals/pull/156

## Changes made to GHC for this

I opened a couple patches to GHC that got merged yesterday:

* [D4904][], which adds a shell script and some ghci scripts for this. Most of
  of `settings.ghci` was written by Csongor Kiss - I found out about them
  via a [helpful mailing list post][mpickering email] from Matthew Pickering. I
  also incorporated some RTS arguments [suggested by Simon Marlow][JaffaCake
  email]. My main additions were making it so that GHCi could run inside of
  GHCi, and putting up a GHC differential.

* [D4986][], which makes some code changes to GHC that allow it all to be loaded
  into GHCi at once. These changes have no effect on the normal build.

[D4904]: https://phabricator.haskell.org/D4904
[D4986]: https://phabricator.haskell.org/D4986
[mpickering email]: https://mail.haskell.org/pipermail/ghc-devs/2018-May/015810.html
[JaffaCake email]: https://mail.haskell.org/pipermail/ghc-devs/2018-June/015844.html

## Why is `-fobject-code` needed?

After [D4904][] was merged, I realized that `-fobject-code` should have been
included in `settings.ghci`, and indeed it was in Csongor's version. Without it,
you get errors like the following:

```haskell
[ 16 of 493] Compiling State            ( compiler/utils/State.hs, interpreted )
Error: bytecode compiler can't handle unboxed tuples and sums.
  Possibly due to foreign import/export decls in source.
  Workaround: use -fobject-code, or compile this module to .o separately.
```

I've opened another patch, [D5015][] which adds this flag and improves how
`-odir` and `-hidir` are specified. I'll update this post once that patch is
merged.

It is unfortunate that `-fobject-code` is required, because it makes the load
times take much longer than they would be otherwise. There are some benefits to
it, though - the result executes faster, and subsequent GHCi loads will use the
object files when possible.

[D5015]: https://phabricator.haskell.org/D5015

## Next steps

* I'm planning to do an optimized build of `ghc-stage2`, and use that instead.
  It may even make sense to have the ghci script use a different path if it
  exists, so that I can keep around an optimized `ghc-stage2`.

* It would be really nice if `-fobject-code` wasn't needed to load code that
  uses `UnboxedTuples`. I have opened [trac#15454][] about adding some automagic
  to GHCi which would intelligently build just enough `-fobject-code` modules to
  handle the unboxed tuples, and use byte-code for everything else.

* Are nightly builds of GHC downloadable from somewhere? If there are, then the
  following workflow could work for newcomers to GHC development:

    - Download nightly build of GHC

    - Load GHC into GHCi in ~10 minutes or less.  I think that this would be *far*
      more appealing than the current recommended approach for newcomers, which is
      something like a 30 or 40 minute build process on my machine.

[trac#15454]: https://ghc.haskell.org/trac/ghc/ticket/15454

## Can we go three layers deep? Does time slow down?

![inception screenshot](/images/inception.jpg)

A natural question is how deep can we go? Can GHCi run inside GHCi inside GHCi?!
It turns out that it works directly, and the nesting works arbitrarily deep:

```haskell
Prelude [inner]> :script utils/ghc-in-ghci/settings.ghci
package flags have changed, resetting and loading new packages...
Prelude [inner]> :load Main
Ok, 493 modules loaded.
Main [inner]> main
GHCi, version 8.7.20180718: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/mgsloan/.ghci
Loaded GHCi configuration from utils/ghc-in-ghci/inner.ghci
Prelude [inner]> :script utils/ghc-in-ghci/settings.ghci
package flags have changed, resetting and loading new packages...
Prelude [inner]> :load Main
Ok, 493 modules loaded.
Main [inner]> main
GHCi, version 8.7.20180718: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/mgsloan/.ghci
Loaded GHCi configuration from utils/ghc-in-ghci/inner.ghci
Prelude [inner]> 1 + 1
2
Prelude [inner]> :q
Leaving GHCi.
Main [inner]> :q
Leaving GHCi.
Main [inner]> :q
Leaving GHCi.
λ :q
Leaving GHCi.
```

4 layers deep of GHCi! This is kinda cheating, though, because it is reusing the
existing object files, and so it isn't really running the compiler.

I've tried removing the object files and loading GHCi inside GHCi inside GHCi,
and it took about 9 minutes to load about 170 modules.  At that point, it
exited with a panic:

```haskell
<no location info>: error:
    ghc-stage2: panic! (the 'impossible' happened)
  (GHC version 8.7.20180718 for x86_64-unknown-linux):
        ASSERT failed!
  $c==_a81xD
  Call stack:
      CallStack (from HasCallStack):
        callStackDoc, called at compiler/utils/Outputable.hs:1164:37 in ghc:Outputable
        pprPanic, called at compiler/utils/Outputable.hs:1223:5 in ghc:Outputable
        assertPprPanic, called at compiler/stgSyn/CoreToStg.hs:991:78 in ghc:CoreToStg
```

This assertion wouldn't have been caught by an optimized build.  It would be
interesting to dig into what's going on there.

On to the question about how much slower this interpreted GHC is. The initial
layering of having an interpreted GHC compile GHC definitely produces a massive
slowdown in compilation speed. However, I hypothesize that each subsequent
nesting would not compound the slowdown, because it's running object code. Even
if the byte-code interpreter was being used, it's written in C, and so is part
of the `ghc-stage2` binary, so the slowdown would not compound.

So pretty much, GHCi nesting does not follow inception's rules of time slow down
:-)
