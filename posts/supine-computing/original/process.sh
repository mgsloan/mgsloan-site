#!/bin/sh -xe

exiv2 rm *.jpg

convert stations/ergoquest.jpg -distort Resize x440 stations/ergoquest_resized.png
montage stations/altwork.png stations/ergoquest_resized.png -tile 2x1 -geometry +0+0 stations.png

../../../scripts/process-jpg.sh *.jpg *.png

rm stations.png stations/ergoquest_resized.png
