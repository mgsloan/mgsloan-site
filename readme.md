[![Build Status][ci-img]][ci]

This is the source code for [my personal site][mgsloan]. It is based quite
directly on [Ruud van Asseldonk's site](ruudva), and forked from the [source
code on github](https://github.com/ruuda/blog). The site uses a custom static
site generator written in Haskell.

The generator includes a tiny templating engine, an html and css minifier, and
an aggressive font subsetter. One of my objectives was to cut all the crap
(which almost by definition includes javascript) without compromising on
design. An average page of my site weighs less than jQuery alone (which
jokingly describes itself as “lightweight footprint”). That includes webfonts.

[mgsloan]:     https://mgsloan.com
[ruudva]:      https://ruudvanasseldonk.com
[ruudva-repo]: https://github.com/ruuda/blog
[ci-img]:      https://travis-ci.org/mgsloan/mgsloan-site.svg
[ci]:          https://travis-ci.org/mgsloan/mgsloan-site

License
-------

Like [Ruud's site](ruudva), the source code for this site is licensed under
version 3 of the the [GNU General Public Licence][gplv3]. See the `licence`
file. The content of the posts is licensed under the [Creative Commons BY
SA][cc] licence. For the font license details, see the readme in the fonts
directory.

[gplv3]: https://gnu.org/licenses/gpl.html
[cc]:    https://creativecommons.org/licenses/by-sa/3.0/

Compiling
---------
Install Python requirements (for font subsetting) in a virtualenv:

    $ pip3 install virtualenv
    $ virtualenv venv
    $ source venv/bin/activate
    $ pip install -r requirements.txt

Note that python 2's virtualenv cannot be used, hence `pip3 install virtualenv`.

Build the generator, then build the site (requires fonts to be present):

    $ stack setup
    $ stack build
    $ stack exec blog

Serving Output Locally
----------------------

Since this already involves installing python3, I (mgsloan) figured may as well
use python's simple web server to test the site locally:

    $ source env/bin/activate
    $ cd out
    $ python3 -m http.server 8080

Other Requirements
------------------

I (mgsloan) needed to install some utilities invoked by this project, and I use
ubuntu. This list is not exhaustive.

    $ sudo apt install zopfli brotli

I am not sure why I needed to install `brotli`, since it is in requirements.txt
