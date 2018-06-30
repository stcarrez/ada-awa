# Check whether we can use gprbuild or gnatmake
AC_DEFUN(AM_GNAT_CHECK_GPRBUILD,
[
  AC_CHECK_PROGS(GPRBUILD, gprbuild, "")
  if test -n "$GPRBUILD"; then
    GNATMAKE="$GPRBUILD"
  else
    AC_CHECK_PROGS(GNATMAKE, gnatmake, "")
  fi

  AC_CHECK_PROGS(GPRCLEAN, gprclean, "")
  if test -n "$GPRCLEAN"; then
    GNATCLEAN="$GPRCLEAN"
  else
    AC_CHECK_PROGS(GNATCLEAN, gnatclean, "")
  fi
])

# Check if a GNAT project is available.
# dnl AM_GNAT_CHECK_PROJECT([name],[path])
AC_DEFUN(AM_GNAT_CHECK_PROJECT,
[
  AC_MSG_CHECKING([whether $1 project exists])
  echo "with \"$2\"; project conftest is for Source_Dirs use (); end conftest;" > conftest.gpr
  if AC_TRY_COMMAND([gnat ls -Pconftest.gpr system.ads > /dev/null 2>conftest.out])
  then
    gnat_project_$1=yes
    AC_MSG_RESULT([yes, using $2])
    gnat_project_with_$1="with \"$2\";";
  else
    gnat_project_$1=no
    AC_MSG_RESULT(no)
  fi
  rm -f conftest.gpr
])

# Check if a GNAT project is available.
# AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],[link],[code-fail],[code-ok])
AC_DEFUN(AM_GNAT_FIND_PROJECT,
[
  AC_MSG_CHECKING([$2])
  AC_ARG_WITH($1,
    AS_HELP_STRING([--with-$1=x], [Path for $2]),
    [
      gnat_project_name_$3=${withval}/
      if test -d "${withval}"; then
	    gnat_project_name_$3=${withval}/$3
	  fi
    ],
    [
      gnat_project_name_$3=$3
    ])
  AC_MSG_RESULT(trying ${gnat_project_name_$3})

  rm -f conftest.gpr
  # Search in the GNAT project path.
  AC_MSG_CHECKING([whether ${gnat_project_name_$3} project exists in gnatmake's search path])
  echo "with \"${gnat_project_name_$3}\"; project conftest is for Source_Dirs use (); end conftest;" > conftest.gpr
  if AC_TRY_COMMAND([gnat ls -Pconftest.gpr system.ads > /dev/null 2>conftest.out])
  then
    gnat_project_$3=yes
    AC_MSG_RESULT(yes, using ${gnat_project_name_$3})
  else
    gnat_project_$3=no
    AC_MSG_RESULT(no)

    # Search in ../$1-*/$3.gpr
    files=`ls -r ../$1/$3.gpr ../$3/$3.gpr ../$1-*/$3.gpr 2>/dev/null`
    for name in $files; do
      dir=`dirname $name`
      AC_MSG_CHECKING([for $2 project in ${dir}])
      echo "with \"${name}\"; project conftest is for Source_Dirs use (); end conftest;" > conftest.gpr
      if AC_TRY_COMMAND([gnat ls -Pconftest.gpr system.ads > /dev/null 2>conftest.out])
      then
         gnat_project_$3=yes
		 gnat_project_name_$3=${name}
         AC_MSG_RESULT(yes, using ${name})
         break
      else
         gnat_project_$3=no
         AC_MSG_RESULT(no)
      fi
    done
  fi
  rm -f conftest.gpr
  if test x${gnat_project_$3} = xyes; then
    gnat_project_with_$3="with \"${gnat_project_name_$3}\";";
    gnat_project_dir_$3=`dirname ${gnat_project_name_$3}`
    if test ${gnat_project_dir_$3} = . ; then
       gnat_project_dir_$3=
    else
       gnat_project_dir_$3="${gnat_project_dir_$3}/"
    fi
    $6
  else
    gnat_project_dir_$3=
    gnat_project_name_$3=
    if test x"$5" != x; then
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

  AC_MSG_RESULT(${ac_enable_shared})
  BUILDS_SHARED=$ac_enable_shared
  AC_SUBST(BUILDS_SHARED)
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
])

dnl Check whether the distrib/debug build is enabled.
AC_DEFUN(AM_DISTRIB_SUPPORT,
[
  AC_MSG_CHECKING([distribution build])
  ac_enable_distrib=yes
  ac_build_mode=distrib
  AC_ARG_ENABLE(distrib,
    [  --enable-distrib        build for distribution, optimized and strip symbols (enabled)],
    [case "${enableval}" in
      no|none)  ac_enable_distrib=no
                ac_build_mode=debug
                ;;
      *)        ac_enable_distrib=yes
                ac_build_mode=distrib
                ;;
    esac])dnl

  AC_MSG_RESULT(${ac_enable_distrib})
  BUILDS_DISTRIB=$ac_enable_distrib
  AC_SUBST(BUILDS_DISTRIB)

  MODE=$ac_build_mode
  AC_SUBST(MODE)
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
    if test x$gnat_project_aws = xno; then
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
  gnat_prefix=
  gnat_xml_inc_dir=
  gnat_xml_ali_dir=
  gnat_xml_lib_dir=
  gnat_xml_prl_dir=

  AC_CHECK_PROGS(GPRINSTALL, gprinstall, "")
  if test x${gnat_xml_ada} = 'x'; then
     gnat_xml_ada=xmlada-config
  fi
  gnat_xml_config=`$gnat_xml_ada --sax 2>/dev/null`

  # echo "Config: $gnat_xml_config"
  for i in $gnat_xml_config; do
	# echo "  Checking $i"
	case $i in
	  -aI*)
	    name=`echo $i | sed -e 's,-aI,,'`
	    dir=`dirname $name`
	    name=`basename $name`
	    if test x$name = "xxmlada"; then
	   	   gnat_xml_inc_dir=$dir
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
	        gnat_xml_ali_dir=$dir
			;;

		  static|relocatable)
		    name=`basename $dir`
		    dir=`dirname $dir`
			if test x$name = "xxmlada"; then
			   gnat_xml_ali_dir=$dir
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
	    gnat_xml_lib_dir=$dir
	    ;;

	/*.a)
		dir=`dirname $i`
	    name=`basename $dir`
		case $name in
		  xmlada)
	        dir=`dirname $dir`
	        gnat_xml_lib_dir=$dir
			;;

		  static|relocatable)
		    dir=`dirname $dir`
		    name=`basename $dir`
			if test x$name = "xxmlada"; then
			   dir=`dirname $dir`
			   gnat_xml_lib_dir=$dir
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
       if test x$gnat_prefix = x; then
          gnat_prefix=$dir
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
    fi
  done

  if test -f $gnat_prefix/lib/gnat/xmlada.gpr ; then
    gnat_xml_prj_dir=$gnat_prefix/lib/gnat
  elif test -f $gnat_xml_inc_dir/xmlada.gpr ; then
    gnat_xml_prj_dir=$gnat_xml_inc_dir
  elif test -f $gnat_prefix/share/gpr/xmlada.gpr ; then
    gnat_xml_prj_dir=$gnat_prefix/share/gpr
  else
    gnat_xml_prj_dir=$gnat_xml_inc_dir
  fi
  if test x${gnat_xml_inc_dir} = x ; then
    gnat_xml_inc_dir='include'
  fi
  if test x${gnat_xml_lib_dir} = x ; then
    gnat_xml_lib_dir='lib'
  fi
  if test x${gnat_xml_ali_dir} = x ; then
    gnat_xml_ali_dir='lib'
  fi
  if test x${gnat_xml_prj_dir} = x ; then
    gnat_xml_prj_dir='lib/gnat'
  fi
  ADA_INC_BASE=`echo $gnat_xml_inc_dir | sed -e s,^$gnat_prefix/,,`
  ADA_LIB_BASE=`echo $gnat_xml_lib_dir | sed -e s,^$gnat_prefix/,,`
  ADA_ALI_BASE=`echo $gnat_xml_ali_dir | sed -e s,^$gnat_prefix/,,`
  ADA_PRJ_BASE=`echo $gnat_xml_prj_dir | sed -e s,^$gnat_prefix/,,`
  AM_UTIL_INSTALL([${gnat_xml_inc_dir}],[${gnat_xml_ali_dir}],[${gnat_xml_lib_dir}],[${gnat_xml_prj_dir}])
])

dnl Guess the installation path
AC_DEFUN(AM_UTIL_CHECK_INSTALL,
[
  AC_CHECK_PROGS(GPRINSTALL, gprinstall, "")
  AM_GNAT_CHECK_PROJECT([util_config],[util_config])

  # Search in the GNAT project path.
  AC_MSG_CHECKING([for util_config.gpr installation])
  # echo "D:${gnat_project_with_util_config}"
  echo "${gnat_project_with_util_config} project t is for Source_Dirs use (); end t;" > t.gpr
  # cat t.gpr
  gnat_util_config_path=`$GNATMAKE -vP1 -Pt 2>&1 | awk '/Parsing.*util_config.gpr/ {print @S|@2}' | sed -e 's,",,g'`
  AC_MSG_RESULT(${gnat_util_config_path})

  gnat_inc_dir=
  gnat_ali_dir=
  gnat_prj_dir=
  gnat_lib_dir=
  if test x${gnat_util_config_path} != x; then
    if test -f ${gnat_util_config_path}; then
      gnat_inc_dir=`awk '/Includedir/ {print @S|@3}' ${gnat_util_config_path} | sed -e 's,",,g' -e 's,;,,'`
      gnat_lib_dir=`awk '/Libdir/ {print @S|@3}' ${gnat_util_config_path} | sed -e 's,",,g' -e 's,;,,'`
      gnat_ali_dir=`awk '/Alidir/ {print @S|@3}' ${gnat_util_config_path} | sed -e 's,",,g' -e 's,;,,'`
      gnat_prj_dir=`dirname ${gnat_util_config_path}`
    fi
  fi
  if test x${gnat_prj_dir} != x; then
    AM_UTIL_INSTALL([${gnat_inc_dir}],[${gnat_ali_dir}],[${gnat_lib_dir}],[${gnat_prj_dir}])
  else
    AM_GNAT_CHECK_INSTALL
  fi
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

   rm -f check.adb check
])

# Prepare for using the GNAT project 
# AM_GNAT_LIBRARY_PROJECT([name])
AC_DEFUN(AM_GNAT_LIBRARY_PROJECT,
[
  # checking for local tools
  AM_GNAT_CHECK_GPRBUILD

  AC_PROG_MAKE_SET
  AC_PROG_INSTALL
  AC_PROG_LN_S
  AM_SHARED_LIBRARY_SUPPORT
  AM_DISTRIB_SUPPORT
  AM_COVERAGE_SUPPORT

  AC_MSG_CHECKING([number of processors])
  NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
  AC_MSG_RESULT($NR_CPUS)
  AC_SUBST(NR_CPUS)

  AM_UTIL_CHECK_INSTALL

  AC_MSG_CHECKING([preparing for GNAT project $1])
  mkdir -p obj/$1/static obj/$1/relocatable lib/$1/static lib/$1/relocatable
  AC_MSG_RESULT(done)
])

