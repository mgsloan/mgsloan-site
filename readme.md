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
> jokingly describes itself as “lightweight footprint”). That includes webfonts.

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

```
$ sudo apt install zopfli brotli
```

# How to do quick iterative writing / modification

I like to be able to modify the Haskell, templates, and markdown, and quickly
see the results. In other words, I want to be able to just save files, and have
the browser automatically update. Here's how to do this:

## Install ghcid and live-server

[ghcid] automates reloading code in `ghci`, Haskell's interpreter. Code gets
reloaded whenever source files change.

[live-server] is a server which modifies HTML it serves, to inject some JS that
will cause the page to reload when the file changes.

```
$ stack install ghcid
$ npm install -g
```

Then, in one terminal run the following:

```
./interpret.sh
```

This script runs `ghcid`, configuring it to run `main` and watch the `posts` and
`templates` directory.

```
./server.sh
```

This script runs `live-server` in the `out` directory. The <localhost:8080>

[ghcid]: https://github.com/ndmitchell/ghcid
[live-server]: https://github.com/tapio/live-server

License
-------

Like [Ruud's site](ruudva), the source code for this site is licensed under
version 3 of the the [GNU General Public Licence][gplv3]. See the `licence`
file. The content of the posts is licensed under the [Creative Commons BY
SA][cc] licence. For the font license details, see the readme in the fonts
directory.

[gplv3]: https://gnu.org/licenses/gpl.html
[cc]:    https://creativecommons.org/licenses/by-sa/3.0/
