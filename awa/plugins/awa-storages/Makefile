ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_storages_dir=$(ROOTDIR)/plugins/awa-storages
awa_storages_dynamo=--package AWA.Storages.Models db ../../uml/awa.zargo
awa_storages_install_dirs=config db bundles web

$(eval $(call awa_plugin,awa_storages))

