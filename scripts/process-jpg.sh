#!/bin/bash -xe

SCRIPT_DIR=`dirname $0`

for ORIGINAL_PATH in "$@"
do
  FILE="${ORIGINAL_PATH#original/}"
  FILE_NO_EXT="${FILE%.*}"

  DEST_PATH="`dirname $ORIGINAL_PATH`/../images"

  mkdir -p "$DEST_PATH"
  "$SCRIPT_DIR/resize-image.sh" "$ORIGINAL_PATH" "$DEST_PATH/$FILE_NO_EXT.jpg"

  # ./resize.sh original/$FILE resized/$FILE
  # /opt/mozjpeg/bin/cjpeg -outfile compressed/$FILE -quality 75 resized/$FILE
done
