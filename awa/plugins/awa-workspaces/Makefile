ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_workspaces_dir=$(ROOTDIR)/plugins/awa-workspaces
awa_workspaces_dynamo=--package AWA.Workspaces.Models db ../../uml/awa.zargo
awa_workspaces_install_dirs=config db bundles web

$(eval $(call awa_plugin,awa_workspaces))

