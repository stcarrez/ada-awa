ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_settings_dir=$(ROOTDIR)/plugins/awa-settings
awa_settings_dynamo=--package AWA.Settings.Models db ../../uml/awa.zargo
awa_settings_install_dirs=config db

$(eval $(call awa_plugin,awa_settings))

