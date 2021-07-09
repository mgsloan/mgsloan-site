#!/bin/sh -xe
exiv2 rm *.jpg
../../../scripts/process-jpg.sh *.jpg
