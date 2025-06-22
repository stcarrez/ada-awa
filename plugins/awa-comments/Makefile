ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_comments_dir=$(ROOTDIR)/plugins/awa-comments
awa_comments_dynamo=--package AWA.Comments.Models db ../../uml/awa.zargo
awa_comments_install_dirs=config db bundles web

$(eval $(call awa_plugin,awa_comments))
