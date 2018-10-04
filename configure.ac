dnl Autoconf configure script for AWA
dnl Copyright 2011-2018 
dnl

AC_INIT(Makefile.in)

AC_CANONICAL_SYSTEM

# Current release settings
AWA_MAJOR_VERSION=1
AWA_MINOR_VERSION=1
AWA_MICRO_VERSION=0
AWA_VERSION=$AWA_MAJOR_VERSION.$AWA_MINOR_VERSION.$AWA_MICRO_VERSION

##########################################
# Check to enable/disable shared library
##########################################
AM_SHARED_LIBRARY_SUPPORT
AM_DISTRIB_SUPPORT
AM_COVERAGE_SUPPORT

AM_GNAT_CHECK_GPRBUILD

# checking for local tools
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# Set the version number of GtkAda
AC_SUBST(AWA_VERSION)
AC_SUBST(AWA_MAJOR_VERSION)
AC_SUBST(AWA_MINOR_VERSION)
AC_SUBST(AWA_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

AC_CHECK_PROG(UNZIP_CHECK,unzip,yes)
if test x"$UNZIP_CHECK" != x"yes" ; then
    AC_MSG_ERROR([Please install unzip before installing AWA.])
fi

AC_CHECK_PROGS(MYSQL, mysql)
AC_CHECK_PROGS(SQLITE3, sqlite3)
AC_CHECK_PROGS(POSTGRESQL, psql)
if test "X$MYSQL" = "X" && test "X$SQLITE3" = "X" && "X$POSTGRESQL" = "X"; then
    AC_MSG_ERROR([Please install either Postgresql, MySQL/MariaDB or SQLite.])
fi

dnl Check if we are running under Windows with msys to use pwd -W which produces Windows paths such as d:/tool instead of /d/tool
AC_CHECK_PROG(awa_has_msys, msysinfo, yes, no)
if test x${awa_has_msys} = xyes; then
   pwd_option="-W"
else
   pwd_option=""
fi

awa_build_root=
awa_xmlada_name=
awa_aws_name=
AC_ARG_WITH(build-root,
  AS_HELP_STRING([--with-build-root=PATH], [Path to find the AWA libraries]),
  [
    awa_build_root=${withval}/
  ],
  [
    awa_build_root=`pwd $pwd_option`/
  ])

awa_build_pwd=`cd ${awa_build_root} && pwd $pwd_option`
if test x${awa_build_pwd} != x${awa_build_root}; then
  awa_build_root=${awa_build_pwd}/
fi

ENABLE_AHVEN=yes
AC_ARG_ENABLE(ahven,
[  --enable-ahven          Enable internal Ahven support for Ada util unit tests (enabled)],
[case "${enableval}" in
  no|none)  ENABLE_AHVEN= ;;
  *)        ENABLE_AHVEN=yes
            ENABLE_AUNIT=no ;;
esac])dnl

AC_ARG_ENABLE(aunit,
[  --enable-aunit          build AUnit provided by AWA],
ENABLE_AUNIT=$enableval,
ENABLE_AUNIT=no)

AC_ARG_ENABLE(xmlada,
[  --enable-xmlada         build XML/Ada provided by AWA],
ENABLE_XMLADA=$enableval,
ENABLE_XMLADA=no)

AC_ARG_ENABLE(aws,
[  --enable-aws            build AWS provided by AWA],
ENABLE_AWS=$enableval,
ENABLE_AWS=no)

AC_SUBST(ENABLE_AHVEN)
AC_SUBST(ENABLE_AUNIT)
AC_SUBST(ENABLE_XMLADA)
AC_SUBST(ENABLE_AWS)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)

if test $ENABLE_AUNIT = "yes" ; then
  PACKAGES="$PACKAGES aunit-gpl-2014-src"
fi

if test $ENABLE_XMLADA = "yes" ; then
  PACKAGES="$PACKAGES xmlada-gpl-4.1-src"
fi

if test $ENABLE_AWS = "yes" ; then
  AC_MSG_CHECKING([for GNAT compiler version])
  version=`$GNATMAKE --version | grep GNATMAKE | sed -e 's,GNATMAKE ,,'`
  case "$version" in
   4.7* | 4.8* | 4.9*)
      AC_MSG_RESULT($version)
      PACKAGES="$PACKAGES aws-gpl-2015-src"
      ;;

   *)
      AC_MSG_RESULT($version)
      PACKAGES="$PACKAGES aws-gpl-2016-src"
      ;;

  esac
fi

dnl Extract the packages if necessary
dnl - AWS, AUnit, XML/Ada are provided in 'external' directory
for NAME in $PACKAGES ; do
   TGZ=$NAME.tar.gz
   EXTRACT_DIR=${awa_build_root}
   DIR=$NAME
   case $NAME in
      xmlada-gpl-4.1-src)
        DIR=xmlada-4.1-src
        TARGET=xmlada
		TGZ=$NAME.tgz
		EXTRACT_DIR=
		XMLADA_SRC_DIR=$DIR
        ;;

      aunit-*)
        TARGET=aunit
		TGZ=$NAME.tgz
        ;;

      aws-*)
        TARGET=$DIR
		TGZ=external/aws/$NAME.tar.gz
		AWS_SRC_DIR=$DIR
        ;;

      *)
        TARGET=$NAME
        ;;
   esac
   if test -f distrib/$TGZ ; then
      TGZ="distrib/$TGZ"
   else
      if test -f external/$TARGET/$TGZ ; then
         TGZ="external/$TARGET/$TGZ"
      fi
   fi
   dnl Do nothing if the target directory exist
   if test -d ${EXTRACT_DIR}${TARGET} ; then
      echo "Package ${TARGET} already extracted."
   else
      echo "Extracting $TGZ"
      tar xzf $TGZ
   fi
done

DIR=`pwd`

##########################################
# Check for XML Ada
##########################################
AM_GNAT_FIND_PROJECT([xmlada],[XML/Ada],[xmlada],
  [http://libre.adacore.com/libre/tools/xmlada/],
  [],
  [
    XMLADA_DIR=${gnat_project_dir_xmlada}
  ])
AC_SUBST(XMLADA_DIR)
AC_SUBST(XMLADA_SRC_DIR)

if test x${gnat_project_xmlada} = xno; then
   if test $ENABLE_XMLADA != "yes" ; then
      AC_MSG_ERROR([XML/Ada was not found and --enable-xmlada not specified])
   fi
   if test ! -d ${XMLADA_SRC_DIR}; then
      AC_MSG_ERROR([XML/Ada extraction was not found in ${XMLADA_SRC_DIR}])
   fi

   echo "Configuring ==== XML/Ada ===="
   (cd ${XMLADA_SRC_DIR};
    ./configure --prefix=${ac_top_build_prefix}
   )
fi

##########################################
# Check for Ada Web Server
##########################################
AM_GNAT_FIND_PROJECT([aws],[Ada Web Server],[aws],
  [http://libre.adacore.com/libre/tools/aws/],
  [],
  [
    AWS_DIR=${gnat_project_dir_aws}
  ])
AC_SUBST(AWS_DIR)
AC_SUBST(AWS_SRC_DIR)

if test x${gnat_project_aws} = xno; then
   if test $ENABLE_AWS != "yes" ; then
      AC_MSG_ERROR([Ada Web Server not found and --enable-aws not specified])
   fi

   NEED_AWS_PATCH=1
   echo "Configuring ==== aws ===="
   (cd $DIR/$AWS_SRC_DIR;
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
         make prefix="$prefix" setup
      )
   AWS_DIR=
fi

AWA_BUILD_ROOT=${awa_build_root}
AC_SUBST(AWA_BUILD_ROOT)

AC_OUTPUT(
Makefile
)
