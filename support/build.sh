#!/bin/sh
# Utility script to help in building various packages from a tarball

. build.config

if test $# -eq 0; then
   echo "Usage: build.sh package-name"
   echo "Example:"
   echo "   build.sh xmlada-gpl-4.1-src"
fi

NAME=$1
FILE=
if test -f $1.tar.gz ; then
   FILE="$1.tar.gz"
elif test -f $1.tgz ; then
   FILE="$1.tgz"
else
   echo "Package $1.tar.gz does not exist."
   exit 1
fi

# Fix the extraction name
case $NAME in
  xmlada-gpl-4.1-src)
     NAME=xmlada-4.1-src
     ;;
esac

# Cleanup and extract the new tarball
rm -rf $NAME
tar xvf $FILE &&
cd $NAME &&

# Configure and build according to the package
case $NAME in
  ada-util-*)
    ./configure --prefix="$PREFIX" &&
    make &&
    make test &&
    gnatmake -Psamples &&
    make install &&
    make install-support
    ;;

  ada-el-*)
    ./configure --prefix="$PREFIX" &&
    make &&
    make test &&
    gnatmake -Psamples &&
    make install
    ;;

  ada-asf-*)
    ./configure --prefix="$PREFIX" &&
    make &&
    make test &&
    gnatmake -Psamples &&
    make install
    ;;

  ada-ado-*)
    ./configure --prefix="$PREFIX" &&
    make &&
    make test &&
    gnatmake -Psamples &&
    make install
    ;;

  dynamo-*)
    ./configure --prefix="$PREFIX" &&
    make &&
    make test &&
    make install
    ;;

  aws-*)
    cp makefile.conf makefile.conf.orig
    sed -e 's,^SOCKET.*=.*,SOCKET=openssl,' \
        -e 's,^IPv6.*=.*,IPv6=true,' < makefile.conf.orig > makefile.conf
    if test $NEED_AWS_PATCH -eq 1 ; then
       # Fix builder switches (others) does not seem to be recognized by gprbuild
       cp tools/tools.gpr tools/tools.gpr.orig
       sed -e 's,(others),("Ada"),g' < tools/tools.gpr.orig > tools/tools.gpr

       # Remove -B option which is not supported by gcc 4.4
       cp shared.gpr shared.gpr.orig
       sed -e 's,gnaty3abBcefhiIklmnoprstx,gnaty3abcefhiIklmnoprstx,' < shared.gpr.orig > shared.gpr
    fi
    make prefix="$PREFIX" setup build install
    ;;

  xmlada-*)
    ./configure --prefix="$PREFIX" &&
    make &&
    make install
    ;;
    
  aunit-*)
    make INSTALL="$PREFIX" &&
    make INSTALL="$PREFIX" install
    ;;
    

esac
