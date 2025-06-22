#!/bin/sh
NAME=awa.cov
alr exec -- lcov --quiet --base-directory . --directory . \
   --no-external \
   --exclude '*/<unknown>' \
   --exclude '*/b__*.adb' \
   --exclude '*/samples/*' \
   --exclude '*/awaunit/*' \
   --exclude '*/ada-util/*' \
   --exclude '*/ada-el/*' \
   --exclude '*/ada-servlet/*' \
   --exclude '*/ada-security/*' \
   --exclude '*/ada-wiki/*' \
   --exclude '*/ada-asf/*' \
   --exclude '*/ada-keystore/*' \
   --exclude '*/openapi-ada/*' \
   --exclude '*/ada-ado/*' \
   --exclude '*/model/*' \
   --exclude '*/regtests*'  -c -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
