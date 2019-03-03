ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_jobs_dir=$(ROOTDIR)/plugins/awa-jobs
awa_jobs_dynamo=--package AWA.Jobs.Models db ../../uml/awa.zargo
awa_jobs_install_dirs=config db

$(eval $(call awa_plugin,awa_jobs))
