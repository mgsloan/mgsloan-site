#!/bin/sh -xe

exiv2 rm *.jpg *.png

# https://stackoverflow.com/a/19475281/1164871
convert oct-tree-1.jpg -rotate 90 oct-tree-1-rotated.png
convert oct-tree-2.jpg -rotate 90 oct-tree-2-rotated.png

# https://stackoverflow.com/a/20749970/1164871
montage oct-tree-1-rotated.png oct-tree-2-rotated.png -tile 2x1 -geometry +0+0 oct-tree-merged.png

montage tree-shadow.jpg sun-face.jpg -tile 2x1 -geometry +0+0 tree-shadow-sun-face.png

convert sitpack-zen-use.jpg -rotate 180 -crop 2268x3024+882+0 +repage sitpack-zen-use.png
convert sitpack-zen-and-tree.jpg -rotate 90 -resize 2268x3024 sitpack-zen-and-tree.png
montage sitpack-zen-use.png sitpack-zen-and-tree.png -tile 2x1 -geometry +0+0 sitpack-zen.png

convert paracord-1.jpg -rotate 90 paracord-1.png
convert paracord-2.jpg -rotate 90 paracord-2.png
montage paracord-*.png -tile 2x1 -geometry +0+0 paracord.png

../../../scripts/process-jpg.sh oct-tree-merged.png starting-tree-based-post.jpg x1-carbon-flat.jpg standing-tree-desk.png x1-zip-ties-around-rope.jpg p51-zip-ties-around-rope.jpg tree-shadow-sun-face.png attach-keyboard-1.png attach-keyboard-2.png attach-keyboard-3.png rock-seat-1.jpg rock-seat-2.jpg sitpack-zen.png paracord.png

rm oct-tree-1-rotated.png oct-tree-2-rotated.png oct-tree-merged.png tree-shadow-sun-face.png sitpack-zen-use.png sitpack-zen-and-tree.png paracord*.png
