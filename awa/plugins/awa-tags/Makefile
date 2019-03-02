ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_tags_dir=$(ROOTDIR)/plugins/awa-tags
awa_tags_dynamo=--package AWA.Tags.Models db $(ROOTDIR)/uml/awa.zargo
awa_tags_install_dirs=config db bundles web

$(eval $(call awa_plugin,awa_tags))

