#!/bin/sh

sed -e 's,@USE_CONVERT@,,' \
    -e 's,@USE_MAGICK@,-unused,' \
    awa/plugins/awa-images/config/images.xml.in > awa/plugins/awa-images/config/images.xml
