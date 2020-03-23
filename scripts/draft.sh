#!/bin/bash -xe

ghcid --run=":main render-draft \"$1\"" &

cd draft/out
python3 -m http.server &
livereload -w 2
