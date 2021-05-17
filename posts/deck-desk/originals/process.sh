#!/bin/sh

exiv2 rm *.jpg

../../../scripts/process-jpg.sh *.jpg
