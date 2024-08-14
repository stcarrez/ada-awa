dnl Autoconf configure script for Ada Web Application
dnl Copyright (C) 2009 - 2024 Stephane Carrez
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

AC_INIT
AC_CONFIG_SRCDIR([Makefile.conf.in])

cache_file=config.cache

# Current release settings
AWA_MAJOR_VERSION=2
AWA_MINOR_VERSION=6
AWA_MICRO_VERSION=0
AWA_VERSION=$AWA_MAJOR_VERSION.$AWA_MINOR_VERSION.$AWA_MICRO_VERSION

AC_CANONICAL_TARGET
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

DIR=`pwd $awa_pwd_option`

ac_configure_args="${ac_configure_args} --with-build-root=${DIR}"
AC_CONFIG_SUBDIRS(ada-lzma ada-util ada-security ada-wiki ada-ado ada-servlet openapi-ada ada-asf ada-keystore dynamo awa)

AC_CONFIG_FILES([Makefile.conf
])
AC_OUTPUT
