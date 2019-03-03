ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_changelogs_dir=$(ROOTDIR)/plugins/awa-changelogs
awa_changelogs_dynamo=--package AWA.Changelogs.Models db ../../uml/awa.zargo
awa_changelogs_install_dirs=config db

$(eval $(call awa_plugin,awa_changelogs))
