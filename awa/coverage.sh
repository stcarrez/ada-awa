#!/bin/sh
NAME=awa.cov
lcov --quiet --base-directory . --directory . -c -o $NAME
lcov --quiet --remove $NAME "/usr*" -o $NAME
lcov --quiet --remove $NAME "/build*" -o $NAME
lcov --quiet --remove $NAME "/opt*" -o $NAME
lcov --quiet --remove $NAME "*/adainclude*" -o $NAME
lcov --quiet --remove $NAME "*/regtests*" -o $NAME
lcov --quiet --remove $NAME "*/b__*" -o $NAME
lcov --quiet --remove $NAME "*/regtests/*" -o $NAME
lcov --quiet --remove $NAME "*/awaunit/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-util/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-el/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-servlet/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-security/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-asf/*" -o $NAME
lcov --quiet --remove $NAME "*/ada-ado/*" -o $NAME
lcov --quiet --remove $NAME "*/openapi-ada/*" -o $NAME
# Ignore generated code
lcov --quiet --remove $NAME "*/model/*" -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
