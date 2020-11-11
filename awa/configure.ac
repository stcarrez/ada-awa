dnl Autoconf configure script for Ada Server Faces
dnl Copyright (C) 2009 - 2020 Stephane Carrez
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

AC_INIT(src/awa.ads)

# Current release settings
AWA_MAJOR_VERSION=2
AWA_MINOR_VERSION=1
AWA_MICRO_VERSION=0
AWA_VERSION=$AWA_MAJOR_VERSION.$AWA_MINOR_VERSION.$AWA_MICRO_VERSION

# checking for local tools
AC_CHECK_PROG(UNZIP_CHECK,unzip,yes)
if test x"$UNZIP_CHECK" != x"yes" ; then
    AC_MSG_ERROR([Please install unzip before installing AWA.])
fi

# Check that Magick or convert are available but do not abort if they are not available
USE_CONVERT="-unused"
USE_MAGICK="-unused"
AC_CHECK_PROG(MAGICK_CHECK,magick,yes)
if test x"$MAGICK_CHECK" != x"yes" ; then
    AC_CHECK_PROG(CONVERT_CHECK,convert,yes)
    if test x"$CONVERT_CHECK" = x"yes" ; then
       USE_CONVERT=""
    fi
else
   USE_MAGICK=""
fi
AC_SUBST(USE_MAGICK)
AC_SUBST(USE_CONVERT)

# Set the version number of AWA
AC_SUBST(AWA_VERSION)
AC_SUBST(AWA_MAJOR_VERSION)
AC_SUBST(AWA_MINOR_VERSION)
AC_SUBST(AWA_MICRO_VERSION)

AWA_LIBDIR="lib"
AC_SUBST(AWA_LIBDIR)

AM_GNAT_LIBRARY_PROJECT(awa)

AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)

AM_GNAT_FIND_ADA_UTIL([Ada Web Application])
AM_GNAT_FIND_ADA_EL([Ada Web Application])
AM_GNAT_FIND_ADA_SECURITY([Ada Web Application])
AM_GNAT_FIND_ADA_SERVLET([Ada Web Application])
AM_GNAT_FIND_ADA_SERVER_FACES([Ada Web Application])
AM_GNAT_FIND_ADA_ADO([Ada Web Application])
AM_GNAT_FIND_ADA_WIKI([Ada Web Application])
AM_GNAT_FIND_ADA_OPENAPI([Ada Web Application])
AM_GNAT_FIND_ADA_KEYSTORE([Ada Web Application])

AC_CHECK_PROGS(MYSQL, mysql)
if test "X$MYSQL" != "X" ; then
  ADO_DB_MYSQL=True
else
  ADO_DB_MYSQL=False
fi
AC_SUBST(ADO_DB_MYSQL)

AC_CHECK_PROGS(SQLITE3, sqlite3)
if test "X$SQLITE3" != "X" ; then
  ADO_DB_SQLITE=True
else
  ADO_DB_SQLITE=False
fi
AC_SUBST(ADO_DB_SQLITE)

AC_CHECK_PROGS(PSQL, psql)
if test "X$PSQL" != "X" ; then
  ADO_DB_POSTGRESQL=True
else
  ADO_DB_POSTGRESQL=False
fi
AC_SUBST(ADO_DB_POSTGRESQL)

##########################################
# Ada Web Server
##########################################
AM_GNAT_CHECK_AWS(
  [
    WITH_SERVER=
    AWA_AWS_STATUS=no
  ], [
    AWA_AWS_STATUS=yes
    WITH_SERVER=$ac_cv_gnat_project_with_aws
  ])

AC_SUBST(AWA_AWS_STATUS)
AC_SUBST(WITH_SERVER)
AM_GNAT_AWS_VERSION

AC_MSG_CHECKING([using AWS secure mail client])
AWA_AWS_SECURE_MAIL=no
case $AWS_VERSION in
  *2015*|*2016*|*2017*|*2018*|*2019*|*202*)
     AWA_AWS_SECURE_MAIL=yes
     ;;

  3.2.0|3.3.2)
     AWA_AWS_SECURE_MAIL=yes
     ;;

  *)
     AWA_AWS_SECURE_MAIL=no
     ;;
esac

AC_MSG_RESULT(${AWA_AWS_SECURE_MAIL})
AC_SUBST(AWA_AWS_SECURE_MAIL)

AC_OUTPUT(
Makefile.conf
awa.gpr
awa_tests.gpr
awa_unit.gpr
plugins/awa-mail/awa_mail.gpr
plugins/awa-sysadmin/awa_sysadmin.gpr
plugins/awa-images/config/images.xml
)
