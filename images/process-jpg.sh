#!/bin/bash -xe

for ORIGINAL_PATH in "$@"
do
  FILE=${ORIGINAL_PATH#original/}

  ./resize.sh original/$FILE compressed/$FILE

  # ./resize.sh original/$FILE resized/$FILE
  # /opt/mozjpeg/bin/cjpeg -outfile compressed/$FILE -quality 75 resized/$FILE
done
