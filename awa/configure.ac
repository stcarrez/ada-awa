dnl Autoconf configure script for Ada Server Faces
dnl Copyright (C) 2009 - 2019 Stephane Carrez
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
AWA_MAJOR_VERSION=1
AWA_MINOR_VERSION=2
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

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

AWA_LIBDIR="lib"
AC_SUBST(AWA_LIBDIR)

AM_GNAT_LIBRARY_PROJECT(awa)

AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)

ADO_USE_GPRBUILD=no
AC_ARG_WITH(gprbuild,
AS_HELP_STRING([--with-gprbuild], [Use gprbuild instead of gnatmake for the compilation]),
[
  ADO_USE_GPRBUILD=${withval}
])
case "$ADO_USE_GPRBUILD" in
  yes)
    AC_CHECK_PROGS(GNATMAKE, gprbuild, "")
    AC_CHECK_PROGS(GNATCLEAN, gprclean, "")
	;;

  *)
	AC_CHECK_PROGS(GNATMAKE, gnatmake, "")
	AC_CHECK_PROGS(GNATCLEAN, gnatclean, "")
   ;;
esac

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_base],
  [git@github.com:stcarrez/ada-util.git],
  [Building Ada Web Application requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_utilada_base}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Expression Language library
##########################################
AM_GNAT_FIND_PROJECT([ada-el],[Ada Expression Language Library],[elada],
  [git@github.com:stcarrez/ada-el.git],
  [Building Ada Web Application requires the Ada EL Library.],
  [
    EL_DIR=${gnat_project_dir_elada}
  ])
AC_SUBST(EL_DIR)

##########################################
# Ada Wiki library
##########################################
AM_GNAT_FIND_PROJECT([ada-wiki],[Ada Wiki Library],[wikiada],
  [git@github.com:stcarrez/ada-wiki.git],
  [Building Ada Web Application requires the Ada Wiki Library.],
  [
    WIKI_DIR=${gnat_project_dir_wikiada}
  ])
AC_SUBST(WIKI_DIR)

##########################################
# Ada Security library
##########################################
AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
  [git@github.com:stcarrez/ada-security.git],
  [Building Ada Web Application requires the Ada Security Library.],
  [
    SECURITY_DIR=${gnat_project_dir_security}
  ])
AC_SUBST(SECURITY_DIR)

##########################################
# Ada Servlet library
##########################################
AM_GNAT_FIND_PROJECT([ada-servlet],[Ada Servlet Library],[servletada],
  [git@github.com:stcarrez/ada-servlet.git],
  [Building Ada Web Application requires the Ada Servlet Library.],
  [
    SERVLET_DIR=${gnat_project_dir_servletada}
  ])
AC_SUBST(SERVLET_DIR)

##########################################
# Ada Server Faces library
##########################################
AM_GNAT_FIND_PROJECT([ada-asf],[Ada Server Faces Library],[asf],
  [git@github.com:stcarrez/ada-asf.git],
  [Building Ada Web Application requires the Ada Server Faces Library.],
  [
    ASF_DIR=${gnat_project_dir_asf}
  ])
AC_SUBST(ASF_DIR)


##########################################
# Ada Database Objects library
##########################################
AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
  [git@github.com:stcarrez/ada-ado.git],
  [Building Ada Web Application requires the Ada Database Objects Library.],
  [
    ADO_DIR=${gnat_project_dir_ado}
  ])
AC_SUBST(ADO_DIR)

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
    WITH_SERVER="with \"${gnat_project_name}\";";
  ])

AC_SUBST(AWA_AWS_STATUS)
AC_SUBST(WITH_SERVER)

AC_MSG_CHECKING([using AWS secure mail client])
ac_enable_aws_secure_mail=yes
AC_ARG_ENABLE(aws-secure-mail,
[  --enable-aws-secure-mail      Enable the support for secure SMTP client by AWS (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_aws_secure_mail=no ;;
  *)        ac_enable_aws_secure_mail=yes ;;
esac])dnl

AC_MSG_RESULT(${ac_enable_aws_secure_mail})
AWA_AWS_SECURE_MAIL=$ac_enable_aws_secure_mail
AC_SUBST(AWA_AWS_SECURE_MAIL)

AC_OUTPUT(
Makefile.conf
awa.gpr
awa_tests.gpr
awa_unit.gpr
plugins/awa-mail/awa_mail.gpr
plugins/awa-images/config/images.xml
)
