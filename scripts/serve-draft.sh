#!/bin/bash -xe

cd draft/out
python3 -m http.server &
livereload -w 2
