NAME=awa

-include Makefile.conf

include Makefile.defaults

# Directories of sub-projects in the build order.
SUBDIRS=ada-lzma ada-util ada-el ada-security
SUBDIRS+=ada-wiki ada-servlet swagger-ada ada-asf
SUBDIRS+=ada-ado dynamo awa

# Build executables for all mains defined by the project.
build test clean:: Makefile.conf
	for i in $(SUBDIRS); do \
	   $(MAKE) -C "$$i" $@ ; \
	done

install uninstall::
	for i in $(SUBDIRS); do \
	   $(MAKE) -C "$$i" $@ ; \
	done

Makefile.conf:
	@echo "Ada Web Application is not configured, running configure..."
	./configure

# Clean, then build executables for all mains defined by the project.
rebuild: clean build

dist::
	for i in $(DIRS); do \
	   cd $$i && git archive -o ../$$i.tar --prefix=$(distdir)/$$i/ HEAD ; \
           cd .. && tar --concatenate --file=$(distdir).tar $$i.tar ; \
           rm -f $$i.tar; \
        done
	gzip $(distdir).tar

sync-configure:
	for i in $(SUBDIRS); do \
       echo "Updating configure in $$i" && \
       cp aclocal.m4 $$i/aclocal.m4 && \
       (cd $$i && autoconf); \
       echo "Updating Makefile.defaults in $$i" && \
       cp Makefile.defaults $$i/Makefile.defaults ; \
    done
