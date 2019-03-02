ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_votes_dir=$(ROOTDIR)/plugins/awa-votes
awa_votes_dynamo=--package AWA.Workspaces.Models db $(ROOTDIR)/uml/awa.zargo
awa_votes_install_dirs=config db web

$(eval $(call awa_plugin,awa_votes))

