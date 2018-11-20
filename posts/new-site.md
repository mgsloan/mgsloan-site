---
title: A new website!
date: 2018-07-27
synopsis: A brief introduction to my new site, and how it works.
run-in: My past attempt at blogging
---

Welcome to my new blog site!  I kept around the old blog [as a single
page](/wordpress), for posterity.

## How the site generation works

I knew I wanted to use static site generation for my blog, and generate simple
HTML that loads fast. Since I'm a big fan of using [Haskell][], I also had a
preference for configuring and extending the site in Haskell.  The most obvious
answer to this would be the [hakyll][] static site generator.

I didn't want to start from scratch - much more efficient to build off someone
else's site! Many hakyll sites publish their source code, and there is a [list
of sites using hakyll][], so I clicked every single one using Hakyll 4 to see
which ones caught my eye.

I noted down quite a few sites, but one that really caught my eye was [Ruud van
Asseldonk's site][]. It has a lot of the attributes I was looking for. In
particular, it prioritizes simplicity and keeping the download size minimal. It
also looks great to me! With the combination of nice typography, the particular
shade of red highlights, and the background color, it really reminded me of
[Tufte's books][], which I thoroughly enjoyed.

So, I took a look at the [code for Ruud's site][], and was a bit surprised to
see that it was using a custom static site generator instead of hakyll. At
first, this put me off using it as a basis, but then I realized that it had
some nice advantages:

* *Simplicity and changeability* - most of the code is pretty straightforward.
  This makes it easy to modify how it all works. I like having all of the logic
  in one spot, in the main repository. The main complicated bit is the clever
  font subsetting, but I don't use that feature.

* *Hacker aesthetic* - Ruud calls his site generator "homemade". There's the
  simple appeal of crafting your own site generator to fit your purposes.

* *Reliable regeneration* - hakyll tries to only regenerate things that need to
  regenerate. This is a nice feature, but I wonder how it handles the behavior
  of your code changing? I'm guessing that you need to manually force it to do a
  rebuild, perhaps by deleting the results. Since Ruud's generator simply
  regenerates everything, I don't need to think about this. The generator also
  starts with the most recent post, which is typically the one you are working
  on. Since the most recent post updates quickly, the overall rebuild speed is
  not a concern for me.

Due to these reasons, I decided to base my site off of Ruud's design and
generator instead of using hakyll. This is not really a criticism of hakyll - I
didn't really give it a fair shot. I find this custom site generator approach to
be quite nice, though.

Ruud kindly gave me the go-ahead to base my site off of his. You can check out
the [code on github](https://github.com/mgsloan/mgsloan-site). I've tweaked the
layout a fair bit, and I'm use [google fonts][] instead of the beautiful and
proprietary [calluna][] family of fonts. Particularly due to the fonts, I think
[his site][Ruud van Asseldonk's site] is substantially better looking than mine
:-)

[Haskell]: https://haskell.org
[hakyll]: https://jaspervdj.be/hakyll
[list of sites using hakyll]: https://jaspervdj.be/hakyll/examples.html
[Ruud van Asseldonk's site]: https://ruudvanasseldonk.com/
[Tufte's books]: https://www.edwardtufte.com/
[code for Ruud's site]: https://github.com/ruuda/blog
[google fonts]: https://fonts.google.com/
[calluna]: https://www.exljbris.com/calluna.html

## Old blog

I kept a single html page around for posterity. You can visit it
if you are curious to see what I was writing about back then.
