#!/bin/sh -xe

exiv2 rm *.jpg

convert early-tree-based-computing-uncropped.jpg -crop 480x480+0+160 +repage early-tree-based-computing.jpg

convert outdoor-treadmill-above-uncropped.jpg -crop 2268x3024+882+0 +repage outdoor-treadmill-above.png

convert outdoor-treadmill-side.jpg -rotate 90 -resize 2268x3024 outdoor-treadmill-side-rotated.png

montage outdoor-treadmill-side-rotated.png outdoor-treadmill-above.png -tile 2x1 -geometry +0+0 outdoor-treadmill.png

../../../scripts/process-jpg.sh early-tree-based-computing.jpg recent-standing.jpg tree-based-writing.jpg polkadot-ergodox.jpg recent-reclining.jpg roost-keyboardio-airport.jpg wardrobe-computing.jpg back-of-suspended-monitor.jpg outdoor-treadmill.png

rm early-tree-based-computing.jpg outdoor-treadmill-above.png outdoor-treadmill-side-rotated.png outdoor-treadmill.png
