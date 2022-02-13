dnl Autoconf configure script for Ada Web Application
dnl Copyright (C) 2009 - 2022 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT(Makefile.conf.in)

cache_file=config.cache

# Current release settings
AWA_MAJOR_VERSION=2
AWA_MINOR_VERSION=4
AWA_MICRO_VERSION=0
AWA_VERSION=$AWA_MAJOR_VERSION.$AWA_MINOR_VERSION.$AWA_MICRO_VERSION

AC_CANONICAL_SYSTEM
AM_GNAT_CHECK_GPRBUILD
AC_CHECK_PROGS(GNAT, gnat, "")
if test -n "$GNAT"; then
  GNATCHECK="$GNAT ls"
  GNATCHECK_ARG=" system.ads"
else
  GNATCHECK="$GNATMAKE"
  GNATCHECK_ARG=""
fi

# checking for local tools
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AM_SHARED_LIBRARY_SUPPORT
AM_DISTRIB_SUPPORT
AM_COVERAGE_SUPPORT

AC_SUBST(AWA_VERSION)
AC_SUBST(AWA_MAJOR_VERSION)
AC_SUBST(AWA_MINOR_VERSION)
AC_SUBST(AWA_MICRO_VERSION)

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
AM_CHECK_HOST_PWD

ENABLE_AHVEN=yes
AC_ARG_ENABLE(ahven,
[  --enable-ahven          Enable internal Ahven support for Ada util unit tests (enabled)],
[case "${enableval}" in
  no|none)  ENABLE_AHVEN= ;;
  *)        ENABLE_AHVEN=yes
            ENABLE_AUNIT=no ;;
esac])dnl

AM_GNAT_FIND_XML_ADA([],[
   AC_MSG_WARN(XML/Ada was not found.)
   AC_MSG_ERROR([Please install XML/Ada, re-configure and re-build AWA.])
 ])

DIR=`pwd $awa_pwd_option`

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

ac_configure_args="${ac_configure_args} --with-build-root=${DIR}"
AC_CONFIG_SUBDIRS(ada-lzma ada-util ada-el ada-security ada-wiki ada-ado ada-servlet openapi-ada ada-asf ada-keystore dynamo awa)

AM_GNAT_AWS_VERSION

AC_OUTPUT(
Makefile.conf
)
