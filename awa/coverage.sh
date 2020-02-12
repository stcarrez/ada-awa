#!/bin/sh
NAME=awa.cov
lcov --base-directory . --directory . -c --include "*/awa/*" -o $NAME
lcov --remove $NAME awa/b__awa_harness.adb -o $NAME
lcov --remove $NAME "/usr*" -o $NAME
lcov --remove $NAME "*/regtests/*" -o $NAME
lcov --remove $NAME "*/awaunit/*" -o $NAME
lcov --remove $NAME "*/ada-util/*" -o $NAME
lcov --remove $NAME "*/ada-el/*" -o $NAME
lcov --remove $NAME "*/ada-servlet/*" -o $NAME
lcov --remove $NAME "*/ada-security/*" -o $NAME
lcov --remove $NAME "*/ada-asf/*" -o $NAME
lcov --remove $NAME "*/ada-ado/*" -o $NAME
rm -rf cover
genhtml --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
