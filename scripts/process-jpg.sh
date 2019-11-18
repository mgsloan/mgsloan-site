#!/bin/bash -xe

SCRIPT_DIR=`dirname $0`

for ORIGINAL_PATH in "$@"
do
  FILE=${ORIGINAL_PATH#original/}

  $SCRIPT_DIR/resize-image.sh $ORIGINAL_PATH `dirname $ORIGINAL_PATH`/../images/$FILE

  # ./resize.sh original/$FILE resized/$FILE
  # /opt/mozjpeg/bin/cjpeg -outfile compressed/$FILE -quality 75 resized/$FILE
done
