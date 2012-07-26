#!/bin/sh

WORKDIR=`mktemp -d`

for SRC in "$@";
do
   BASE=`basename $SRC`;
   DST="$WORKDIR/$BASE.png"
   echo "$SRC -> $DST";
   convert $SRC $DST
done

mencoder "mf://$WORKDIR/*.png" -mf fps=25:type=png -ovc copy \
    -oac copy -o output.avi

# mencoder "mf://$WORKDIR/*.png" -mf fps=25:type=png -ovc lavc \
#     -lavcopts vcodec=mpeg4:mbd=2:trell -oac copy -o output.avi

rm -rf "$WORKDIR"

