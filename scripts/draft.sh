#!/bin/bash -xe

ghcid --run=":main render-draft \"$1\"" &

cd out-drafts
python3 -m http.server &
livereload -w 0.3
