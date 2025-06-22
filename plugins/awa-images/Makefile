ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_images_dir=$(ROOTDIR)/plugins/awa-images
awa_images_dynamo=--package AWA.Images.Models db ../../uml/awa.zargo
awa_images_install_dirs=config db web

$(eval $(call awa_plugin,awa_images))
