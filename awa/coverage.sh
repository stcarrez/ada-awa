#!/bin/sh
NAME=awa.cov
lcov --quiet --base-directory . --directory . -c --include "*/awa/*" -o $NAME
lcov --quiet --remove $NAME awa/b__awa_harness.adb -o $NAME
lcov --quiet --remove $NAME "/usr*" -o $NAME
lcov --quiet --remove $NAME "*/regtests/*" -o $NAME
lcov --quiet --remove $NAME "*/awaunit/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-util/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-el/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-servlet/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-security/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-asf/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-ado/*" -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
