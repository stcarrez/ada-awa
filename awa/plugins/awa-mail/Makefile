
NAME=awa-mail

LIBNAME=lib${NAME}

include ../../Make-Config.mk

GPRPATH=awa_mail.gpr

# Build executables for all mains defined by the project.
pre-build:	src/awa-mail-clients-factory.adb

# Configure the driver file
src/awa-mail-clients-factory.adb: src/awa-mail-clients-factory.gpb Makefile
	gnatprep -DHAVE_AWS=$(HAVE_AWS) \
		  src/awa-mail-clients-factory.gpb $@
