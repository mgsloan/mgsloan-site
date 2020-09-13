#!/bin/sh -xe

exiv2 rm *.jpg

montage desk-messy-cables-1.jpg desk-messy-cables-2.jpg -tile 2x1 -geometry +0+0 desk-messy-cables-merged.jpg

montage desk-setup.jpg desk-tidy-cables-side.jpg -tile 2x1 -geometry +0+0 desk-tidy-cables.jpg

montage desk-walking.png desk-standing.png -tile 2x1 -geometry +0+0 desk-walking-and-standing.jpg

montage desk-leaning.png desk-sitting.png -tile 2x1 -geometry +0+0 desk-leaning-and-sitting.jpg

montage desk-drinking-water.png desk-drinking-coffee.png -tile 2x1 -geometry +0+0 desk-drinking.jpg

../../../scripts/process-jpg.sh *.jpg

rm ../images/desk-messy-cables-1.jpg
rm ../images/desk-messy-cables-2.jpg
rm desk-messy-cables-merged.jpg

rm ../images/desk-tidy-cables-side.jpg
rm desk-tidy-cables.jpg

rm desk-walking-and-standing.jpg
rm desk-leaning-and-sitting.jpg
