#!/bin/sh

# Inspired directly by https://github.com/ruuda/blog/blob/master/images/compress.sh

mozjpeg='/opt/mozjpeg/bin/cjpeg -quality'

mkdir -p compressed

# inception.jpg is already quite compressed so just copy it.
cp original/inception.jpg compressed/inception.jpg
