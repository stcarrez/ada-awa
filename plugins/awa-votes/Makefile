ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_votes_dir=$(ROOTDIR)/plugins/awa-votes
awa_votes_dynamo=--package AWA.Votes.Models db ../../uml/awa.zargo
awa_votes_install_dirs=config db web

$(eval $(call awa_plugin,awa_votes))

