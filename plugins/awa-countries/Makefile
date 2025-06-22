ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_countries_dir=$(ROOTDIR)/plugins/awa-countries
awa_countries_dynamo=--package AWA.Countries.Models db ../../uml/awa.zargo
awa_countries_install_dirs=db

$(eval $(call awa_plugin,awa_countries))
