[![Build Status][ci-img]][ci]

This is the source code for [my personal site](mgsloan). It is based directly on
[the code](ruudva-repo) for [Ruud van Asseldonk's site](ruudva). The site uses a
custom static site generator written in Haskell.

Here's a description of the generator written by Ruud van Asseldonk, from his
[readme.md]:

> The generator includes a tiny templating engine, an html and css minifier, and
> an aggressive font subsetter. One of my objectives was to cut all the crap
> (which almost by definition includes javascript) without compromising on
> design. An average page of my site weighs less than jQuery alone (which
> describes itself as “lightweight footprint”). That includes webfonts.

I have disabled the clever webfont subsetting support, because instead I am
using some popular fonts served by google. My reasoning is that for many users
these will be cached.

[mgsloan]:     https://mgsloan.com
[ruudva]:      https://ruudvanasseldonk.com
[ruudva-repo]: https://github.com/ruuda/blog
[readme.md]:   https://github.com/ruuda/blog/blob/master/readme.md
[ci-img]:      https://travis-ci.org/mgsloan/mgsloan-site.svg
[ci]:          https://travis-ci.org/mgsloan/mgsloan-site

# Installing dependencies

## stack

`stack` is a tool for building Haskell code. For many unix-ey systems, the
following will do:

```
$ curl -sSL https://get.haskellstack.org/ | sh
```

Otherwise see [the installation docs](stack-install). Once this is done, run
stack setup within this directory to install ghc:

```
$ stack setup
```

[stack-install]: https://docs.haskellstack.org/en/stable/README/#how-to-install

## Compression tools

By default these aren't needed - only needed if relevant code in `Main.hs` is
uncommented.

```
$ sudo apt install zopfli brotli
```

# How to do quick iterative writing / modification

I like to be able to modify the Haskell, templates, and markdown, and quickly
see the results. In other words, I want to be able to just save files, and have
the browser automatically update. My current approach is a bit of a mishmash of
tools. I'd like something better, but it works well enough. Here's how I'm
currently doing this:

## Install ghcid and livereload

[ghcid] automates reloading code in `ghci`, Haskell's interpreter. Code gets
reloaded whenever source files change.

[livereload] is an node server that can be used with the [livereload chrome
plugin] to cause the browser to reload after changes.

```
$ stack install ghcid
$ npm install -g livereload
```

## Run ghcid

Then, in one terminal run the following:

```
$ ghcid
```

This runs `ghcid`, which reads the `.ghcid` file, configuring it to run `main`
and watch a few directories for changes.

## Run python3 http server

Then in another terminal, run the following:

```
$ cd out
$ python3 -m http.server
```

This runs a web server that serves the output of site generation.

## Run livereload

Then, in yet another terminal, run the following:

```
$ cd out
$ livereload
```

[ghcid]: https://github.com/ndmitchell/ghcid
[livereload]: https://www.npmjs.com/package/livereload
[livereload chrome plugin]: https://chrome.google.com/webstore/detail/livereload/jnihajbhpnppcggbcgedagnkighmdlei

License
-------

Like [Ruud's site](ruudva), the source code for this site is licensed under
version 3 of the the [GNU General Public Licence][gplv3]. See the `licence`
file. The content of the posts is licensed under the [Creative Commons BY
SA][cc] licence. For the font license details, see the readme in the fonts
directory.

[gplv3]: https://gnu.org/licenses/gpl.html
[cc]:    https://creativecommons.org/licenses/by-sa/4.0/
