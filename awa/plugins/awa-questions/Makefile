ROOTDIR ?= ../..

include $(ROOTDIR)/Makefile.defaults

awa_questions_dir=$(ROOTDIR)/plugins/awa-questions
awa_questions_dynamo=--package AWA.Questions.Models db ../../uml/awa.zargo
awa_questions_install_dirs=config db bundles web

$(eval $(call awa_plugin,awa_questions))

