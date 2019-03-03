ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_counters_dir=$(ROOTDIR)/plugins/awa-counters
awa_counters_dynamo=--package AWA.Counters.Models db ../../uml/awa.zargo
awa_counters_install_dirs=config db

$(eval $(call awa_plugin,awa_counters))

