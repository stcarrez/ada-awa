#!/usr/bin/make -f
# Build script for XML/Ada in Debian.
# Copyright (c) 2003-2009 Ludovic Brenta <lbrenta@debian.org>
# Copyright (c) 2013-2018 Nicolas Boulenguez <nicolas@debian.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# On Debian systems, the full text of the GPL is in the file
# /usr/share/common-licenses/GPL-3.


# NOTE:
# In order to prevent a circular build-dependency, the Debian packages
# for XML/Ada and GPRBuild bother to build without project support.
# Please keep the two debian/rules files similar enough to ease
# backport of good ideas.

DEB_BUILD_MAINT_OPTIONS := hardening=+all
include /usr/share/dpkg/default.mk
include /usr/share/ada/debian_packaging-$(GNAT_VERSION).mk

ADAFLAGS += -gnatafno -gnatVa

# Ignore gnat.adc in top directory.
BUILDER_OPTIONS += -gnatA

# Rewrite upstream configure/make build system.
.PHONY: $(addprefix override_dh_auto_, \
  configure build-arch build-indep test install clean)

######################################################################
ADS_DIR := usr/share/ada/adainclude
GPR_DIR := usr/share/gpr
ALI_DIR := usr/lib/$(DEB_HOST_MULTIARCH)/ada/adalib
SO_DIR  := usr/lib/$(DEB_HOST_MULTIARCH)
A_DIR   := usr/lib/$(DEB_HOST_MULTIARCH)

define library_template
# Parameters:
# _SRCDIR: directory containing the sources files
# _deps_int       : used in -aI -aO -l for libs built by this deb source
# _deps_int_direct: subset used for .gpr and ada:Depends
# _deps_ext       : used in -aI -aO -l for libs from packaged Build-Depends
# _deps_ext_direct: subset used for .gpr and ada:Depends
# _deps_gnarl     : used in -l, should be empty or gnarl-BV
# For any dependency d, $(d)_DEV_PKG: deb -dev package for $(d)
# For any internal dependency d, $(d)_SRCDIR as above.

  override_dh_install::
  # dynamic library
	chrpath -d lib/$(library)/relocatable/$($(library)_SONAME)
	dh_install --package=$($(library)_LIB_PKG) \
	  lib/$(library)/relocatable/$($(library)_SONAME) \
	  $(SO_DIR)
  # static library
	test -z "$($(library)_ANAME)" || \
	dh_install --package=$($(library)_DEV_PKG) \
	  lib/$(library)/static/$($(library)_ANAME) \
	  $(A_DIR)
  # development symbolic link
	dh_link --package=$($(library)_DEV_PKG) \
	  $(SO_DIR)/$($(library)_SONAME) \
	  $(SO_DIR)/$($(library)_LNAME).so
  # ALI files
	dh_install --package=$($(library)_DEV_PKG) \
	  lib/$(library)/relocatable/*.ali \
	  $(ALI_DIR)/$(library)
  # Sources
	for dir in $($(library)_SRCDIR); do \
	  dh_install --package=$($(library)_DEV_PKG) \
	     $$$$dir/*.ad[bs] $(ADS_DIR)/$(library); \
	done
  # Substitution variables for the control file.
	echo 'ada:Depends=gnat, gnat-$(GNAT_VERSION), \
	  $(foreach l, $($(library)_deps_int_direct), $($(l)_DEV_PKG) (=$$$${binary:Version}),) \
	  $(foreach l, $($(library)_deps_ext_direct), $($(l)_DEV_PKG),) \
	  $($(library)_LIB_PKG)' \
	  >> debian/$($(library)_DEV_PKG).substvars

  # Externally built project installed for end users.
  override_dh_auto_build-arch: obj/$($(library)_GPR)
  obj/$($(library)_GPR): distrib/$($(library)_GPR) | obj
	sed -e s,LIBRARY_TYPE,dynamic, $$< > $$@

  override_dh_install::
	dh_install --package=$($(library)_DEV_PKG) \
	  obj/$($(library)_GPR) \
	  $(GPR_DIR)

endef

define awa_plugin_template
# Parameters:
# _SRCDIR: directory containing the sources files
# _deps_int       : used in -aI -aO -l for libs built by this deb source
# _deps_int_direct: subset used for .gpr and ada:Depends
# _deps_ext       : used in -aI -aO -l for libs from packaged Build-Depends
# _deps_ext_direct: subset used for .gpr and ada:Depends
# _deps_gnarl     : used in -l, should be empty or gnarl-BV
# For any dependency d, $(d)_DEV_PKG: deb -dev package for $(d)
# For any internal dependency d, $(d)_SRCDIR as above.

  override_dh_install::
  # dynamic library
	chrpath -d $($(library)_DIR)/lib/$(library)/relocatable/$($(library)_SONAME)
	dh_install --package=$($(library)_LIB_PKG) \
	  $($(library)_DIR)/lib/$(library)/relocatable/$($(library)_SONAME) \
	  $(SO_DIR)
  # static library
	test -z "$($(library)_ANAME)" || \
	dh_install --package=$($(library)_DEV_PKG) \
	  $($(library)_DIR)/lib/$(library)/static/$($(library)_ANAME) \
	  $(A_DIR)
  # development symbolic link
	dh_link --package=$($(library)_DEV_PKG) \
	  $(SO_DIR)/$($(library)_SONAME) \
	  $(SO_DIR)/$($(library)_LNAME).so
  # ALI files
	dh_install --package=$($(library)_DEV_PKG) \
	  $($(library)_DIR)/lib/$(library)/relocatable/*.ali \
	  $(ALI_DIR)/$(library)
  # Sources
	for dir in $($(library)_SRCDIR); do \
	  dh_install --package=$($(library)_DEV_PKG) \
	     $$$$dir/*.ad[bs] $(ADS_DIR)/$(library); \
	done
  # Substitution variables for the control file.
	echo 'ada:Depends=gnat, gnat-$(GNAT_VERSION), \
	  $(foreach l, $($(library)_deps_int_direct), $($(l)_DEV_PKG) (=$$$${binary:Version}),) \
	  $(foreach l, $($(library)_deps_ext_direct), $($(l)_DEV_PKG),) \
	  $($(library)_LIB_PKG)' \
	  >> debian/$($(library)_DEV_PKG).substvars

  # Externally built project installed for end users.
  override_dh_install::
	sed -e s,LIBRARY_TYPE,dynamic, \
	   $($(library)_DIR)/distrib/$($(library)_GPR) \
	   > $($(library)_DIR)/obj/$($(library)_GPR)
	dh_install --package=$($(library)_DEV_PKG) \
	  $($(library)_DIR)/obj/$($(library)_GPR) \
	  $(GPR_DIR)

endef

obj/shared obj/static: | obj
obj obj/shared obj/static:
	mkdir $@
override_dh_auto_clean::
	rm -fr obj
