#############################################################
#
#  Macro to add for using GNU gettext
#
#############################################################


AC_DEFUN(AM_WITH_NLS,
  [AC_MSG_CHECKING([whether NLS is requested])
    dnl Default is enabled NLS
    AC_ARG_ENABLE(nls,
      [  --disable-nls           do not use Native Language Support],
      USE_NLS=$enableval, USE_NLS=yes)
    AC_MSG_RESULT($USE_NLS)
    AC_SUBST(USE_NLS)

    GETTEXT_INTL="False"
    HAVE_GETTEXT="False"

    dnl If we use NLS figure out what method
    if test "$USE_NLS" = "yes"; then
      AC_DEFINE(ENABLE_NLS)

      dnl Figure out whether gettext is available in the C or intl library.
      nls_cv_header_intl=
      nls_cv_header_libgt=

      AC_CACHE_CHECK([for gettext in libc], gt_cv_func_gettext_libc,
       [AC_TRY_LINK([extern int gettext(char*);], [return (int) gettext ("")],
	gt_cv_func_gettext_libc=yes, gt_cv_func_gettext_libc=no)])

      if test "$gt_cv_func_gettext_libc" != "yes"; then
        AC_CHECK_LIB(intl, bindtextdomain,
         [AC_CACHE_CHECK([for gettext in libintl],
           gt_cv_func_gettext_libintl,
           [AC_CHECK_LIB(intl, gettext,
              gt_cv_func_gettext_libintl=yes,
              gt_cv_func_gettext_libintl=no)],
	    gt_cv_func_gettext_libintl=no)])

	  if test "$gt_cv_func_gettext_libintl" = "yes"; then
            GETTEXT_INTL="True"
          fi
      fi

       if test "$gt_cv_func_gettext_libc" = "yes" \
         || test "$gt_cv_func_gettext_libintl" = "yes"; then
            HAVE_GETTEXT="True"
       fi
    fi

    dnl Make all variables we use known to autoconf.
    AC_SUBST(GETTEXT_INTL)
    AC_SUBST(HAVE_GETTEXT)
  ])

AC_DEFUN(AM_GNU_GETTEXT,
  [AM_WITH_NLS
  ])

dnl Check if we are running under Windows with msys to use pwd -W which produces Windows paths such as d:/tool instead of /d/tool
AC_DEFUN(AM_CHECK_HOST_PWD,
[
  if test x${awa_host_pwd_check} != xyes; then
    case "${target_os}" in
       mingw32*|cygwin*|mingw64*|msys)
       awa_pwd_option="-W"
       ;;

     *)
      awa_pwd_option=""
      ;;

    esac
    awa_host_pwd_check=yes
  fi
])

# Check whether we can use gprbuild or gnatmake
AC_DEFUN(AM_GNAT_CHECK_GPRBUILD,
[
  AC_CHECK_PROGS(GPRBUILD, gprbuild, "")
  if test -n "$GPRBUILD"; then
    GNATMAKE="$GPRBUILD"
  else
    AC_CHECK_PROGS(GNATMAKE, gnatmake, "")
  fi

  if test -z "$GNATMAKE"; then
    AC_MSG_ERROR([gnatmake or gprbuild must be installed.])
  fi;

  AC_CHECK_PROGS(GPRCLEAN, gprclean, "")
  if test -n "$GPRCLEAN"; then
    GNATCLEAN="$GPRCLEAN"
  else
    AC_CHECK_PROGS(GNATCLEAN, gnatclean, "")
  fi

  AC_CHECK_PROGS(GPRINSTALL, gprinstall, "")
])

# Check if a GNAT project is available.
# dnl AM_GNAT_CHECK_PROJECT([name],[path])
AC_DEFUN(AM_GNAT_CHECK_PROJECT,
[
  AC_CACHE_CHECK([whether $1 project exists],[ac_cv_gnat_project_$1],[
    echo "with \"$2\"; project conftest is for Source_Dirs use (); end conftest;" > conftest.gpr
    if AC_TRY_COMMAND([$GNATCHECK -Pconftest.gpr $GNATCHECK_ARG > /dev/null 2>conftest.out])
    then
      ac_cv_gnat_project_$1=yes
      ac_cv_gnat_project_with_$1="with \"$2\";";
    else
      ac_cv_gnat_project_$1=no
    fi
    rm -f conftest.gpr])
])

# Check if a GNAT project is available.
# AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],[link],[code-fail],[code-ok])
AC_DEFUN(AM_GNAT_FIND_PROJECT,
[
  AC_ARG_WITH($1,
    AS_HELP_STRING([--with-$1=x], [Path for $2]),
    [
      if test "${withval}/" = "yes/"; then
        ac_cv_gnat_project_name_$3=${awa_build_root}$1
      else
        ac_cv_gnat_project_name_$3=${withval}/
        if test -d "${withval}"; then
	      ac_cv_gnat_project_name_$3=${withval}/$3
	    fi
      fi
    ],
    [
      ac_cv_gnat_project_name_$3=${awa_build_root}$3
    ])

  AC_CACHE_CHECK([$2],[ac_cv_gnat_project_$3],[
    rm -f conftest.gpr
    # Search in the GNAT project path.
    echo "with \"${ac_cv_gnat_project_name_$3}\"; project conftest is for Source_Dirs use (); end conftest;" > conftest.gpr
    if AC_TRY_COMMAND([$GNATCHECK -Pconftest.gpr $GNATCHECK_ARG > /dev/null 2>conftest.out])
    then
      ac_cv_gnat_project_$3=yes
    else
      ac_cv_gnat_project_$3=no

      # Search in ../$1-*/$3.gpr
      dir=`cd .. && pwd ${awa_pwd_option}`
      files=`ls -r $dir/$1/$3.gpr $dir/$3/$3.gpr $dir/$1-*/$3.gpr 2>/dev/null`
      for name in $files; do
        dir=`dirname $name`
        # AC_MSG_CHECKING([for $2 project in ${dir}])
        echo "with \"${name}\"; project conftest is for Source_Dirs use (); end conftest;" > conftest.gpr
        if AC_TRY_COMMAND([$GNATCHECK -Pconftest.gpr $GNATCHECK_ARG > /dev/null 2>conftest.out])
        then
           ac_cv_gnat_project_$3=yes
		   ac_cv_gnat_project_name_$3=${name}
           # AC_MSG_RESULT(yes, using ${name})
           break
        else
           ac_cv_gnat_project_$3=no
           # AC_MSG_RESULT(no)
        fi
      done
      if test x${ac_cv_gnat_project_$3} != xyes; then
        echo "with \"$3\"; project conftest is for Source_Dirs use (); end conftest;" > conftest.gpr
        if AC_TRY_COMMAND([$GNATCHECK -Pconftest.gpr $GNATCHECK_ARG > /dev/null 2>conftest.out])
        then
           ac_cv_gnat_project_$3=yes
           ac_cv_gnat_project_name_$3=$3
        else
           ac_cv_gnat_project_$3=no
        fi
      fi
    fi
    rm -f conftest.gpr

    if test x${ac_cv_gnat_project_$3} = xyes; then
      ac_cv_gnat_project_with_$3="with \"${ac_cv_gnat_project_name_$3}\";";
      ac_cv_gnat_project_dir_$3=`dirname ${ac_cv_gnat_project_name_$3}`
      if test ${ac_cv_gnat_project_dir_$3} = . ; then
        ac_cv_gnat_project_dir_$3=
      else
        ac_cv_gnat_project_dir_$3="${ac_cv_gnat_project_dir_$3}/"
      fi
    else
      ac_cv_gnat_project_dir_$3=
      ac_cv_gnat_project_name_$3=
    fi
  ])

  if test x${ac_cv_gnat_project_$3} = xyes; then
    $6
  else
    if test x"$5" != x; then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR([$5
  You should build and install the $2 component.
  It must be available and found by ${GNATMAKE}.
  This project was not found in the ADA_PROJECT_PATH environment variable.
  This project was not found in ../$3 nor in ../$1-*.
  The component is available at $4.
  Please, download and configure $2.
  The current configuration was using:
    ${GNATMAKE}
    ADA_PROJECT_PATH=$ADA_PROJECT_PATH
])
    fi
  fi
])

dnl Check for utilada_base GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_UTIL,
[
  AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_base],
    [git@github.com:stcarrez/ada-util.git],
    [Building $1 requires the Ada Utility Library.],
    [
      UTIL_DIR=${ac_cv_gnat_project_dir_utilada_base}
    ])
  AC_SUBST(UTIL_DIR)
])

dnl Check for elada GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_EL,
[
  AM_GNAT_FIND_PROJECT([ada-el],[Ada Expression Language Library],[elada],
    [git@github.com:stcarrez/ada-el.git],
    [Building $1 requires the Ada EL Library.],
    [
      EL_DIR=${ac_cv_gnat_project_dir_elada}
    ])
  AC_SUBST(EL_DIR)
])

dnl Check for security GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_SECURITY,
[
  AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
    [git@github.com:stcarrez/ada-security.git],
    [Building $1 requires the Ada Security Library.],
    [
      SECURITY_DIR=${ac_cv_gnat_project_dir_security}
    ])
  AC_SUBST(SECURITY_DIR)
])

dnl Check for servletada GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_SERVLET,
[
  AM_GNAT_FIND_PROJECT([ada-servlet],[Ada Servlet Library],[servletada],
    [git@github.com:stcarrez/ada-servlet.git],
    [Building $1 requires the Ada Servlet Library.],
    [
      SERVLET_DIR=${ac_cv_gnat_project_dir_servletada}
    ])
  AC_SUBST(SERVLET_DIR)
])

dnl Check for asf GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_SERVER_FACES,
[
  AM_GNAT_FIND_PROJECT([ada-asf],[Ada Server Faces],[asf],
    [git@github.com:stcarrez/ada-asf.git],
    [Building $1 requires the Ada Server Faces Library.],
    [
      ASF_DIR=${ac_cv_gnat_project_dir_asf}
    ])
  AC_SUBST(ASF_DIR)
])

dnl Check for ado GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_ADO,
[
  AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
    [git@github.com:stcarrez/ada-ado.git],
    [Building $1 requires the Ada Database Objects Library.],
    [
      ADO_DIR=${ac_cv_gnat_project_dir_ado}
    ])
  AC_SUBST(ADO_DIR)
])

dnl Check for wikiada GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_WIKI,
[
  AM_GNAT_FIND_PROJECT([ada-wiki],[Ada Wiki Library],[wikiada],
    [git@github.com:stcarrez/ada-wiki.git],
    [Building $1 requires the Ada Wiki Library.],
    [
      WIKI_DIR=${ac_cv_gnat_project_dir_wikiada}
    ])
  AC_SUBST(WIKI_DIR)
])

dnl Check for swaggerada GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_OPENAPI,
[
  AM_GNAT_FIND_PROJECT([openapi-ada],[OpenAPI Ada Library],[openapi],
    [git@github.com:stcarrez/swagger-ada.git],
    [Building $1 requires the Ada OpenAPI Library.],
    [
      OPENAPI_DIR=${ac_cv_gnat_project_dir_openapi}
    ])
  AC_SUBST(OPENAPI_DIR)
])

dnl Check for swaggerada GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_KEYSTORE,
[
  AM_GNAT_FIND_PROJECT([ada-keystore],[Ada Keystore Library],[keystoreada],
    [git@github.com:stcarrez/ada-keystore.git],
    [Building $1 requires the Ada Keystore Library.],
    [
      KEYSTOREADA_DIR=${ac_cv_gnat_project_dir_keystoreada}
    ])
  AC_SUBST(KEYSTOREADA_DIR)
])

dnl Check for AWA GNAT project
AC_DEFUN(AM_GNAT_FIND_ADA_AWA,
[
  AM_GNAT_FIND_PROJECT([awa],[Ada Web Application],[awa],
    [git@github.com:stcarrez/ada-awa.git],
    [Building $1 requires the Ada Web Application Library.],
    [
      AWA_DIR=${ac_cv_gnat_project_dir_awa}
    ])
  AC_SUBST(AWA_DIR)
])

dnl Check for XML/Ada_base GNAT project
dnl AM_GNAT_FIND_PROJECT([code-found],[not-found])
AC_DEFUN(AM_GNAT_FIND_XML_ADA,
[
  gnat_xml_ada=xmlada-config
  AC_ARG_WITH(xmlada,
  AS_HELP_STRING([--with-xmlada=], [Path for XML/Ada]),
  [
    if test T${withval} = Tno ; then
      HAVE_XML_ADA=no;
    else
      gnat_xml_ada=${withval}/xmlada-config;
      WITH_XML_ADA="with \"${withval}\";";
      HAVE_XML_ADA='yes';
    fi
  ],
  [
    WITH_XML_ADA='';
    HAVE_XML_ADA='yes';
  ])

  if test T$HAVE_XML_ADA = Tyes ; then

    AM_GNAT_CHECK_PROJECT([xmlada_sax],[xmlada_sax])
    if test T$ac_cv_gnat_project_xmlada_sax = Tno; then
      AM_GNAT_CHECK_PROJECT([xmlada],[xmlada])
    fi

    AC_CACHE_CHECK([XML/Ada version],[ac_cv_gnat_xmlada_version],[
      if test T$HAVE_XML_ADA = Tyes ; then
        gnat_xmlada_version=`$gnat_xml_ada --version 2>/dev/null | sed -e 's, ,-,g'`
      else
        gnat_xmlada_version=none
        HAVE_XML_ADA='no'
      fi

      case $gnat_xmlada_version in
      XmlAda-3.2*)
        ac_cv_gnat_xmlada_version='3'
        ;;

      XmlAda-4.*|XmlAda-2013|XmlAda-2014)
        ac_cv_gnat_xmlada_version='4'
        ;;

      *)
        ac_cv_gnat_xmlada_version='none'
        HAVE_XML_ADA='no'
        ;;

      esac

      if test T$ac_cv_gnat_project_xmlada_sax = Tno; then
        if test T$ac_cv_gnat_project_xmlada != Tyes; then
          ac_cv_gnat_xmlada_version='none'
          HAVE_XML_ADA='no'
        fi
      else
        ac_cv_gnat_xmlada_version='4'
        HAVE_XML_ADA='yes'
      fi

    ])
  else
    ac_cv_gnat_project_xmlada_sax='no'
  fi

  if test T$ac_cv_gnat_project_xmlada = Tyes; then
    WITH_XML_ADA="with \"xmlada\";";
  fi
  if test T$ac_cv_gnat_project_xmlada_sax = Tyes; then
    WITH_XML_ADA="with \"xmlada_sax\";";
  fi

  VERSION_XML_ADA=$ac_cv_gnat_xmlada_version

  case T$HAVE_XML_ADA in
    Tyes)
      $1
      ;;

    Tno)
      WITH_XML_ADA='';
      VERSION_XML_ADA='none';
      HAVE_XML_ADA='no'
      $2
      ;;

  esac

  AC_SUBST(WITH_XML_ADA)
  AC_SUBST(VERSION_XML_ADA)
  AC_SUBST(HAVE_XML_ADA)
])

dnl Check whether the shared library support is enabled.
AC_DEFUN(AM_SHARED_LIBRARY_SUPPORT,
[
  AC_MSG_CHECKING([shared library support])
  ac_enable_shared=no
  AC_ARG_ENABLE(shared,
    [  --enable-shared         Enable the shared libraries (disabled)],
    [case "${enableval}" in
      no|none)  ac_enable_shared=no ;;
      *)        ac_enable_shared=yes ;;
    esac])dnl
  ac_enable_default_shared=no
  AC_ARG_ENABLE(default-shared,
    [  --enable-default-shared Use shared libraries by default (disabled)],
    [case "${enableval}" in
      no|none)  ac_enable_default_shared=no ;;
      *)        ac_enable_default_shared=yes ;;
    esac])dnl

  AC_MSG_RESULT(${ac_enable_shared})
  BUILDS_SHARED=$ac_enable_shared
  AC_SUBST(BUILDS_SHARED)

  AC_MSG_CHECKING([default library type])
  if test ${ac_enable_shared} = yes && test ${ac_enable_default_shared} = yes; then
    DEFAULT_LIBRARY_TYPE='relocatable'
  else
    DEFAULT_LIBRARY_TYPE='static'
  fi
  AC_MSG_RESULT(${DEFAULT_LIBRARY_TYPE})
  AC_SUBST(DEFAULT_LIBRARY_TYPE)
])

dnl Check whether the coverage support is enabled.
AC_DEFUN(AM_COVERAGE_SUPPORT,
[
  AC_MSG_CHECKING([coverage support])
  ac_enable_coverage=no
  AC_ARG_ENABLE(coverage,
    [  --enable-coverage       build with coverage support -fprofile-arcs -ftest-coverage (disabled)],
    [case "${enableval}" in
      no|none)  ac_enable_coverage=no ;;
      *)        ac_enable_coverage=yes ;;
    esac])dnl

  AC_MSG_RESULT(${ac_enable_coverage})
  BUILDS_COVERAGE=$ac_enable_coverage
  AC_SUBST(BUILDS_COVERAGE)
  if test T$ac_enable_coverage = Tyes; then
     ac_build_mode='coverage'
  fi
])

dnl Check whether the distrib/debug build is enabled.
AC_DEFUN(AM_DISTRIB_SUPPORT,
[
  AC_MSG_CHECKING([distribution build])
  ac_enable_distrib=yes
  ac_quiet_mode=-q
  ac_build_mode=distrib
  AC_ARG_ENABLE(distrib,
    [  --enable-distrib        build for distribution, optimized and strip symbols (enabled)],
    [case "${enableval}" in
      no|none)  ac_enable_distrib=no
                ac_build_mode=debug
                ac_quiet_mode=
                ;;
      *)        ac_enable_distrib=yes
                ac_build_mode=distrib
                ac_quiet_mode=-q
                ;;
    esac])dnl

  AC_MSG_RESULT(${ac_enable_distrib})
  BUILDS_DISTRIB=$ac_enable_distrib
  AC_SUBST(BUILDS_DISTRIB)

  BUILDS_QUIET=$ac_quiet_mode
  AC_SUBST(BUILDS_QUIET)
])

dnl Check whether the AWS support is enabled and find the aws GNAT project.
AC_DEFUN(AM_GNAT_CHECK_AWS,
[
  dnl Define option to enable/disable AWS
  gnat_enable_aws=yes
  gnat_project_aws=no
  gnat_project_name_aws=
  AC_ARG_ENABLE(aws,
    [  --enable-aws            Enable the AWS support (enabled)],
    [case "${enableval}" in
      no|none)  gnat_enable_aws=no ;;
      *)        gnat_enable_aws=yes ;;
    esac])dnl

  AC_MSG_CHECKING([AWS support is enabled])
  AC_MSG_RESULT(${gnat_enable_aws})

  if test T$gnat_enable_aws = Tyes; then
    dnl AC_MSG_NOTICE([Ada Web Server library (http://libre.adacore.com/libre/tools/aws/)])
    AC_ARG_WITH(aws,
    AS_HELP_STRING([--with-aws=x], [Path for the Ada Web Server library (http://libre.adacore.com/libre/tools/aws/)]),
    [
      gnat_project_name=${withval}
    ],
    [
      gnat_project_name=aws
    ])
    AM_GNAT_CHECK_PROJECT([aws],[${gnat_project_name}])
    if test x$ac_cv_gnat_project_aws = xno; then
      gnat_enable_aws=no
    else
      gnat_project_aws=aws
    fi
  fi
  if test T$gnat_enable_aws = Tno; then
    $1
  else
	$2
  fi
])

dnl Setup installation paths
dnl AM_UTIL_INSTALL([inc],[ali],[lib],[prj])
AC_DEFUN(AM_UTIL_INSTALL,
[
  gnat_prefix=
  for dir in $1 $2 $3 $4; do
    dir=`echo $dir | sed -e 's,\\\\,/,g'`
    # If we have a valid path, try to identify the common path prefix.
    if test x$gnat_prefix = x; then
      gnat_prefix=$dir
    else
	  # echo "Dir=$dir"
	  gnat_old_ifs=$IFS
	  path=
	  IFS='/\'
	  for c in $dir; do
	    if test x"$path" = x"/" || test x"$path" = x ; then
		  case $c in
		    c:|C:|d:|D:|e:|E:)
			  try="$c"
			  ;;
		    *)
			  try="/$c"
			  ;;
		  esac
		else
          try="$path/$c"
		fi
		# echo "gnat_prefix=$gnat_prefix try=$try path=$path c=$c"
		case $gnat_prefix in
		  $try*)
			;;
		  *)
			break
			;;
		esac
		  path=$try
	  done
	  IFS=$gnat_old_ifs
	  gnat_prefix=$path
    fi
  done
  ADA_INC_BASE=`echo $1 | sed -e 's,\\\\,/,g' | sed -e s,^$gnat_prefix/,,`
  ADA_ALI_BASE=`echo $2 | sed -e 's,\\\\,/,g' | sed -e s,^$gnat_prefix/,,`
  ADA_LIB_BASE=`echo $3 | sed -e 's,\\\\,/,g' | sed -e s,^$gnat_prefix/,,`
  ADA_PRJ_BASE=`echo $4 | sed -e 's,\\\\,/,g' | sed -e s,^$gnat_prefix/,,`

  AC_MSG_CHECKING([installation of Ada source files])
  AC_MSG_RESULT(<prefix>/${ADA_INC_BASE})

  AC_MSG_CHECKING([installation of Ada ALI files])
  AC_MSG_RESULT(<prefix>/${ADA_ALI_BASE})

  AC_MSG_CHECKING([installation of library files])
  AC_MSG_RESULT(<prefix>/${ADA_LIB_BASE})

  AC_MSG_CHECKING([installation of GNAT project files])
  AC_MSG_RESULT(<prefix>/${ADA_PRJ_BASE})

  AC_SUBST(ADA_INC_BASE)
  AC_SUBST(ADA_LIB_BASE)
  AC_SUBST(ADA_ALI_BASE)
  AC_SUBST(ADA_PRJ_BASE)
])


dnl Check by using xmlada-config where some files are installed.
dnl The goad is to find or guess some installation paths.
dnl           XML/Ada                    Debian
dnl *.ads     <prefix>/include/xmlada    <prefix>/usr/share/adainclude/xmlada  
dnl *.ali     <prefix>/lib/xmlada/static <prefix>/usr/lib/<arch>/ada/adalib/xmlada
dnl *.so      <prefix>/lib/xmlada/static <prefix>/usr/lib/<arch>
dnl *.prj     <prefix>/lib/gnat          <prefix>/usr/share/adainclude

AC_DEFUN(AM_GNAT_CHECK_INSTALL,
[
  #
  ac_cv_gnat_prefix=
  ac_cv_gnat_xml_inc_dir=
  ac_cv_gnat_xml_ali_dir=
  ac_cv_gnat_xml_lib_dir=
  ac_cv_gnat_xml_prl_dir=

  if test x${ac_cv_gnat_xml_ada} = 'x'; then
     ac_cv_gnat_xml_ada=xmlada-config
  fi
  ac_cv_gnat_xml_config=`$gnat_xml_ada --sax 2>/dev/null`

  # echo "Config: $gnat_xml_config"
  for i in $ac_cv_gnat_xml_config; do
	# echo "  Checking $i"
	case $i in
	  -aI*)
	    name=`echo $i | sed -e 's,-aI,,'`
	    dir=`dirname $name`
	    name=`basename $name`
	    if test x$name = "xxmlada"; then
	   	   ac_cv_gnat_xml_inc_dir=$dir
		else
		   dir=''
	    fi
	    ;;

	 -aO*)
	    name=`echo $i | sed -e 's,-aO,,'`
	    dir=`dirname $name`
	    name=`basename $name`
		case $name in
		  xmlada)
	        ac_cv_gnat_xml_ali_dir=$dir
			;;

		  static|relocatable)
		    name=`basename $dir`
		    dir=`dirname $dir`
			if test x$name = "xxmlada"; then
			   ac_cv_gnat_xml_ali_dir=$dir
			else
			   dir=''
			fi
		    ;;

		  *)
		    dir=''
			;;

		esac
	    ;;

	-largs)
	    dir=''
		;;

     -L*)
	    dir=`echo $i | sed -e 's,-L,,'`
	    ac_cv_gnat_xml_lib_dir=$dir
	    ;;

	/*.a)
		dir=`dirname $i`
	    name=`basename $dir`
		case $name in
		  xmlada)
	        dir=`dirname $dir`
	        ac_cv_gnat_xml_lib_dir=$dir
			;;

		  static|relocatable)
		    dir=`dirname $dir`
		    name=`basename $dir`
			if test x$name = "xxmlada"; then
			   dir=`dirname $dir`
			   ac_cv_gnat_xml_lib_dir=$dir
			else
			   dir=''
			fi
		    ;;

		  *)
		    dir=''
			;;

		esac		
		;;

     *)
	    dir=
	    ;;
    esac

    # If we have a valid path, try to identify the common path prefix.
    if test x$dir != "x"; then
       if test x$ac_cv_gnat_prefix = x; then
          ac_cv_gnat_prefix=$dir
       else
	   # echo "Dir=$dir"
	   gnat_old_ifs=$IFS
	   path=
	   IFS=/
	   for c in $dir; do
	      if test x"$path" = x"/"; then
		    try="/$c"
		  else
			try="$path/$c"
		  fi
		  # echo "gnat_prefix=$gnat_prefix try=$try path=$path c=$c"
		  case $ac_cv_gnat_prefix in
		    $try*)
			   ;;
		    *)
			   break
			   ;;
		  esac
		  path=$try
	   done
	   IFS=$gnat_old_ifs
	   ac_cv_gnat_prefix=$path
       fi
    fi
  done

  if test -f $ac_cv_gnat_prefix/lib/gnat/xmlada.gpr ; then
    ac_cv_gnat_xml_prj_dir=$ac_cv_gnat_prefix/lib/gnat
  elif test -f $gnat_xml_inc_dir/xmlada.gpr ; then
    ac_cv_gnat_xml_prj_dir=$ac_cv_gnat_xml_inc_dir
  elif test -f $ac_cv_gnat_prefix/share/gpr/xmlada.gpr ; then
    ac_cv_gnat_xml_prj_dir=$ac_cv_gnat_prefix/share/gpr
  else
    ac_cv_gnat_xml_prj_dir=$gnat_xml_inc_dir
  fi
  if test x${ac_cv_gnat_xml_inc_dir} = x ; then
    ac_cv_gnat_xml_inc_dir='include'
  fi
  if test x${ac_cv_gnat_xml_lib_dir} = x ; then
    ac_cv_gnat_xml_lib_dir='lib'
  fi
  if test x${ac_cv_gnat_xml_ali_dir} = x ; then
    ac_cv_gnat_xml_ali_dir='lib'
  fi
  if test x${ac_cv_gnat_xml_prj_dir} = x ; then
    ac_cv_gnat_xml_prj_dir='lib/gnat'
  fi
  ADA_INC_BASE=`echo $ac_cv_gnat_xml_inc_dir | sed -e s,^$ac_cv_gnat_prefix/,,`
  ADA_LIB_BASE=`echo $ac_cv_gnat_xml_lib_dir | sed -e s,^$ac_cv_gnat_prefix/,,`
  ADA_ALI_BASE=`echo $ac_cv_gnat_xml_ali_dir | sed -e s,^$ac_cv_gnat_prefix/,,`
  ADA_PRJ_BASE=`echo $ac_cv_gnat_xml_prj_dir | sed -e s,^$ac_cv_gnat_prefix/,,`
  AM_UTIL_INSTALL([${ac_cv_gnat_xml_inc_dir}],[${ac_cv_gnat_xml_ali_dir}],[${ac_cv_gnat_xml_lib_dir}],[${ac_cv_gnat_xml_prj_dir}])
])

# AM_TRY_ADA and AM_HAS_INTRINSIC_SYNC_COUNTERS are imported from GNATcoll aclocal.m4
#############################################################
# Check whether gnatmake can compile, bind and link an Ada program
#    AM_TRY_ADA(gnatmake,filename,content,success,failure)
#############################################################

AC_DEFUN(AM_TRY_ADA,
[
   cat > conftest.ada <<EOF
[$3]
EOF
   if AC_TRY_COMMAND([gnatchop -q conftest.ada && $1 $2 >/dev/null 2>conftest.out])
   then
      : Success
      $4
   else
      : Failure
      $5
   fi
   rm -rf conftest.ada
])

#############################################################
# Check whether platform/GNAT supports atomic increment/decrement
# operations.
# The following variable is then set:
#     SYNC_COUNTERS_IMPL
# to either "intrinsic" or "mutex"
# Code comes from the PolyORB configure.ac
#############################################################

AC_DEFUN(AM_HAS_INTRINSIC_SYNC_COUNTERS,
[
  AC_MSG_CHECKING([whether platform supports atomic inc/dec])
  AM_TRY_ADA([gnatmake], [check.adb],
[
with Interfaces; use Interfaces;
procedure Check is
   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32;
   pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_4");
   X : aliased Interfaces.Integer_32;
   Y : Interfaces.Integer_32 := 0;
   pragma Volatile (Y);
   --  On some platforms (e.g. i386), GCC has limited support for
   --  __sync_add_and_fetch_4 for the case where the result is not used.
   --  Here we want to test for general availability, so make Y volatile to
   --  prevent the store operation from being discarded.
begin
   Y := Sync_Add_And_Fetch (X'Access, 1);
end Check;
],
[
   AC_MSG_RESULT(yes)
   $1
],[
   AC_MSG_RESULT(no)
   $2
])

   rm -f check.adb check check.o check.ali
])

# Prepare for using the GNAT project 
# AM_GNAT_LIBRARY_SETUP([name])
AC_DEFUN(AM_GNAT_LIBRARY_SETUP,
[
  AC_MSG_CHECKING([preparing for GNAT project $1])
  mkdir -p obj/$1/static obj/$1/relocatable lib/$1/static lib/$1/relocatable
  AC_MSG_RESULT(done)
])

# Prepare for using the GNAT project 
# AM_GNAT_LIBRARY_PROJECT([name])
AC_DEFUN(AM_GNAT_LIBRARY_PROJECT,
[
  AC_ARG_WITH(build-root,
    AS_HELP_STRING([--with-build-root=PATH], [Path to find the Ada libraries]),
    [
      awa_build_root=${withval}/
    ],
    [
      awa_build_root=''
    ])

  if test x${awa_build_root} != x; then
    AM_CHECK_HOST_PWD
    awa_build_pwd=`cd ${awa_build_root} && pwd $awa_pwd_option`
    if test x${awa_build_pwd} != x${awa_build_root}; then
      awa_build_root=${awa_build_pwd}/
    fi
  fi

  # checking for local tools
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

  AC_PROG_MAKE_SET
  AC_PROG_INSTALL
  AC_PROG_LN_S
  AM_SHARED_LIBRARY_SUPPORT
  AM_DISTRIB_SUPPORT
  AM_COVERAGE_SUPPORT

  BUILD=$ac_build_mode
  AC_SUBST(BUILD)
  
  AC_CACHE_CHECK([number of processors],[ac_cv_proc_count],[
    ac_cv_proc_count=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
  ])
  NR_CPUS=$ac_cv_proc_count
  AC_SUBST(NR_CPUS)

  AM_GNAT_LIBRARY_SETUP($1)
])

dnl Check and retrieve the Ada Web Server version
dnl HTTP Delete is supported after 2017
AC_DEFUN(AM_GNAT_AWS_VERSION,
[
  AC_CACHE_CHECK([checking AWS version],[ac_cv_gnat_aws_version],[

    cat > conftest.adb <<EOF
with AWS;
with Ada.Text_IO;
procedure Conftest is
begin
  Ada.Text_IO.Put_Line (AWS.Version);
end Conftest;
EOF

    cat > conftest.gpr <<EOF
with "aws";
project t is
  for Main use ("conftest.adb");
end t;
EOF

    if AC_TRY_COMMAND([gnatmake -Pconftest.gpr >/dev/null 2>conftest.out])
    then
       ac_cv_gnat_aws_version=`./conftest`
    else
       ac_cv_gnat_aws_version='none'
    fi

    rm -f conftest conftest.o conftest.ali
    if test "$ac_cv_gnat_aws_version" = "20.0"; then
      dnl The version 20.0 is sometimes wrong because used by several versions of AWS.
      dnl The version 22.0 has the AWS.HTTP_2 constant defined and uses version 20.0.
      dnl Check for that and fix the version.
      cat > conftest.adb <<EOF
with AWS;
with Ada.Text_IO;
procedure Conftest is
begin
  Ada.Text_IO.Put_Line (AWS.HTTP_2);
end Conftest;
EOF
      if AC_TRY_COMMAND([gnatmake -Pconftest.gpr >/dev/null 2>conftest.out])
      then
         ac_cv_gnat_aws_version="22.0"
      fi

    fi

    rm -f conftest.gpr conftest.adb conftest.o conftest.ali
    rm -f b__conftest.ads b__conftest.adb b__conftest.o b__conftest.ali
  ])

  AWS_VERSION=$ac_cv_gnat_aws_version
  AC_SUBST(AWS_VERSION)
])

dnl Check the OS and CPU to build Ada Util configuration (UTIL_OS)
AC_DEFUN(AM_ADA_UTIL_HARDWARE,
[
  AC_CANONICAL_TARGET

  os_base='unix'
  os_version='none'
  AC_MSG_CHECKING([operating system])
  case "${target_os}" in
     linux|linux-*|solaris*|sysv-*)
        os_version='linux'
        ;;

     netbsd*|dragonfly*)
        os_version='netbsd'
        ;;

     openbsd*|freebsd*)
        # Let OpenBSD people cry 
        os_version='freebsd'
        ;;

     macos*|darwin*)
        os_version='macos'
        ;;

     mingw32*|cygwin*|mingw64*|msys)
        os_version='win'
        os_base='windows'
        ;;

     mingw32*|cygwin*|mingw64*|msys)
        os_version='win'
        os_base='windows'
        ;;

     *)
        # Be authoritative
        os_version='linux'
        ;;
  esac
  AC_MSG_RESULT($os_version)

  AC_MSG_CHECKING([hardware platform])
  case "${target_cpu}" in
     x86_64)
        os_version="${os_version}64"
        HARDWARE_PLATFORM=${target_cpu}
        ;;

     i386|i486|i586|i686)
        os_version="${os_version}32"
        HARDWARE_PLATFORM='x86'
        ;;

     armv8*|aarch64*|mips64*|mipsisa64*|sh64*|riscv64*|sparc64*)
        os_version="${os_version}64"
        HARDWARE_PLATFORM=${target_cpu}
        ;;

     armv7*|mips*|mipsisa*|sh*|riscv32*|sparc*)
        os_version="${os_version}32"
        HARDWARE_PLATFORM=${target_cpu}
        ;;

     *)
        os_version="${os_version}64"
        HARDWARE_PLATFORM=${target_cpu}
        ;;
  esac
  AC_MSG_RESULT($HARDWARE_PLATFORM)
  AC_SUBST(HARDWARE_PLATFORM)

  # Check for gcc intrinsics
  AM_HAS_INTRINSIC_SYNC_COUNTERS(src_asm='intrinsic',src_asm='')
  AC_MSG_CHECKING([specific processor support])
  if test T$src_asm = T; then
   case "${target}" in
   ## Intel 386 machines where we don't care about the manufacturer
     i[[34567]]86-*-* | x86_* | x86-*)
       src_asm='x86'
       ;;

     *)
       src_asm='none'
       ;;

   esac
  fi
  AC_MSG_RESULT(using $src_asm)
  UTIL_ASM_TYPE="$src_asm"
  AC_SUBST(UTIL_ASM_TYPE)

  UTIL_OS_VERSION=$os_version
  AC_SUBST(UTIL_OS_VERSION)

])

dnl Identify the AWS version for Ada Util.
AC_DEFUN(AM_ADA_UTIL_AWS_VERSION,
[
AM_GNAT_CHECK_AWS(
  [
    UTIL_HAVE_AWS=no
    WITH_SERVER="";
    WITH_UTIL_AWS="";
    UTIL_AWS_VERSION="none"
  ], [
    UTIL_HAVE_AWS=yes
    WITH_UTIL_AWS="with \"utilada_aws\";";
    WITH_SERVER=$ac_cv_gnat_project_with_aws
    AM_GNAT_LIBRARY_SETUP(utilada_aws)

    AM_GNAT_AWS_VERSION

    AC_MSG_CHECKING([using Ada Util AWS http client])
    UTIL_AWS_VERSION=1
case $AWS_VERSION in
  22.0)
     UTIL_AWS_VERSION=3
     ;;

  *2017*|*2018*|*2019*|*202*|20.0)
     UTIL_AWS_VERSION=2
     ;;

  3.3.2)
     UTIL_AWS_VERSION=2
     ;;

  3.*|2.*)
     UTIL_AWS_VERSION=1
     ;;

  *)
     UTIL_AWS_VERSION=3
     ;;
esac
    AC_MSG_RESULT(${UTIL_AWS_VERSION})

  ])

AC_SUBST(UTIL_AWS_VERSION)

])
