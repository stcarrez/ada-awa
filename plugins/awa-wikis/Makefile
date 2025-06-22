ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_wikis_dir=$(ROOTDIR)/plugins/awa-wikis
awa_wikis_dynamo=--package AWA.Wikis.Models db ../../uml/awa.zargo
awa_wikis_install_dirs=config db bundles web

$(eval $(call awa_plugin,awa_wikis))
