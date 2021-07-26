#!/bin/sh
# Usage: awa-wiki-preview.sh <STYLE-SHEET> <HTML> <PNG>
if [ $# -ne 3 ]; then
  echo "Usage: awa-wiki-preview.sh <STYLE-SHEET> <HTML> <PNG>" 1>&2
  exit 1
fi
DIR=`dirname $3`
mkdir -p $DIR &&
wkhtmltoimage -q --enable-local-file-access --user-style-sheet $1 --zoom 1.5 --crop-h 512 --crop-w 1024 $2 $3.tmp.png &&
convert $3.tmp.png -resize 256x128 $3 &&
rm -f $3.tmp.png

