ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_sysadmin_dir=$(ROOTDIR)/plugins/awa-sysadmin
awa_sysadmin_dynamo=--package AWA.Sysadmin.Models db ../../uml/awa.zargo
awa_sysadmin_install_dirs=config db web

$(eval $(call awa_plugin,awa_sysadmin))

generate::
	rm -rf $(ROOTDIR)/plugins/awa-sysadmin/db/sqlite \
               $(ROOTDIR)/plugins/awa-sysadmin/db/mysql \
               $(ROOTDIR)/plugins/awa-sysadmin/db/postgresql
