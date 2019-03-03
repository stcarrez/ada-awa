ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_blogs_dir=$(ROOTDIR)/plugins/awa-blogs
awa_blogs_dynamo=--package AWA.Blogs.Models db ../../uml/awa.zargo
awa_blogs_install_dirs=config db bundles web

$(eval $(call awa_plugin,awa_blogs))

