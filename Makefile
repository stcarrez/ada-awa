NAME=awa

-include Makefile.conf

include Makefile.defaults

# Directories of sub-projects to package in the .tar.gz distribution
DIST_DIRS=ada-lzma ada-util ada-el ada-security
DIST_DIRS+=ada-wiki ada-servlet swagger-ada ada-asf
DIST_DIRS+=ada-ado dynamo

# Directories of sub-projects in the build order.
SUBDIRS=ada-lzma ada-util ada-el ada-security
SUBDIRS+=ada-wiki ada-servlet swagger-ada ada-asf
SUBDIRS+=ada-ado dynamo awa

# Build executables for all mains defined by the project.
build test clean::
	for i in $(SUBDIRS); do \
	   $(MAKE) -C "$$i" $@ ; \
	done

install install-data uninstall::
	for i in $(SUBDIRS); do \
	   $(MAKE) -C "$$i" $@ ; \
	done

# Clean, then build executables for all mains defined by the project.
rebuild: clean build

dist::
	rm -f $(DIST_FILE)
	git archive -o $(DIST_DIR).tar --prefix=$(DIST_DIR)/ HEAD
	for i in $(DIST_DIRS); do \
	   cd $$i && git archive -o ../$$i.tar --prefix=$(DIST_DIR)/$$i/ HEAD ; \
           cd .. && tar --concatenate --file=$(DIST_DIR).tar $$i.tar ; \
           rm -f $$i.tar; \
        done
	gzip $(DIST_DIR).tar

sync-configure:
	for i in $(SUBDIRS); do \
       echo "Updating configure in $$i" && \
       cp aclocal.m4 $$i/aclocal.m4 && \
       (cd $$i && autoconf); \
       echo "Updating Makefile.defaults in $$i" && \
       cp Makefile.defaults $$i/Makefile.defaults ; \
    done

ifeq ($(BUILDS_SHARED),yes)
install-shared:
endif