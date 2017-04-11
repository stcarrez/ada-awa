#!/bin/sh
lcov --base-directory . --directory . -c -o awa.cov
bin/awa_harness -p SQLite -xml awa-sqlite-aunit.xml -config test-sqlite.properties
bin/awa_harness -p MySQL -xml awa-mysql-aunit.xml -config test-mysql.properties
lcov --base-directory . --directory . -c -o awa.cov
lcov --remove awa.cov awa/b__awa_harness.adb -o awa.cov
lcov --remove awa.cov "/usr*" -o awa.cov
lcov --remove awa.cov "regtests*" -o awa.cov
lcov --remove awa.cov "awaunit*" -o awa.cov
lcov --remove awa.cov "ada-util/*" -o awa.cov
lcov --remove awa.cov "ada-ado/*" -o awa.cov
lcov --remove awa.cov "ada-asf/*" -o awa.cov
lcov --remove awa.cov "ada-el/*" -o awa.cov
lcov --remove awa.cov "ada-security/*" -o awa.cov
rm -rf cover
genhtml -o ./cover -t "test coverage" --num-spaces 4 awa.cov
 
