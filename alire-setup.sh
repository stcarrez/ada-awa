#!/bin/sh

CONVERT=`which convert`
USE_CONVERT="-unused"
USE_MAGICK="-unused"
if test x"$CONVERT" != x; then
  USE_CONVERT=""
else
  MAGICK=`which magick`
  if test x"$MAGICK" != x; then
    USE_MAGICK=""
  fi
fi
sed -e "s,@USE_CONVERT@,$USE_CONVERT,g" \
    -e "s,@USE_MAGICK@,$USE_MAGICK,g" \
    awa/plugins/awa-images/config/images.xml.in > awa/plugins/awa-images/config/images.xml
