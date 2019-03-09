ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_mail_dir=$(ROOTDIR)/plugins/awa-mail
awa_mail_install_dirs=web

$(eval $(call awa_plugin,awa_mail))

setup:: $(awa_mail_dir)/src/awa-mail-clients-factory.adb

# Configure the driver file
$(awa_mail_dir)/src/awa-mail-clients-factory.adb: $(awa_mail_dir)/src/awa-mail-clients-factory.gpb $(ROOTDIR)/Makefile.conf
	gnatprep -DHAVE_AWS=$(HAVE_AWS) \
	   $(awa_mail_dir)/src/awa-mail-clients-factory.gpb $@
