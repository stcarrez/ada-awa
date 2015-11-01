#!/bin/sh
# Usage: awa-wiki-preview.sh <STYLE-SHEET> <HTML> <PNG>
if [ $# -ne 3 ]; then
  echo "Usage: awa-wiki-preview.sh <STYLE-SHEET> <HTML> <PNG>" 1>&2
  exit 1
fi
TMP=$3.tmp
wkhtmltoimage -q --user-style-sheet $1 --crop-h 512 --crop-w 512 $2 $3.tmp &&
convert $3.tmp -resize 128x128 $3 &&
rm -f $3.tmp

