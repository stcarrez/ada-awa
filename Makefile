NAME=awa

-include Makefile.conf

include Makefile.defaults

# Directories of sub-projects to package in the .tar.gz distribution
DIST_DIRS=ada-lzma ada-util ada-el ada-security
DIST_DIRS+=ada-wiki ada-servlet openapi-ada ada-asf ada-keystore
DIST_DIRS+=ada-ado dynamo

# Directories of sub-projects in the build order.
SUBDIRS=ada-lzma ada-util ada-el ada-security
SUBDIRS+=ada-wiki ada-servlet openapi-ada ada-asf ada-keystore
SUBDIRS+=ada-ado dynamo awa

# Build executables for all mains defined by the project.
build test clean dist-clean::
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

# run the coverage reports and send them to codecov.io
# it assumes that CODECOV_TOKEN_<submodule> is defined in environment variable.
update-coverage:
	curl -s https://codecov.io/bash -o codecov-io.sh
	for i in $(SUBDIRS); do \
      if test -f $$i/coverage.sh; then \
        echo "Update coverage for $$i" && \
        (cd $$i && sh ./coverage.sh && \
         file=`grep '^NAME=' ./coverage.sh | sed -e 's,NAME=,,'` && \
         token=`echo $$file | sed -e 's,.cov,,'` && \
         codecov_token=`eval echo \\$$CODECOV_TOKEN_$$token` && \
         commit=`git rev-parse HEAD` && \
         export TRAVIS_COMMIT=$$commit && \
         export TRAVIS_REPO_SLUG=stcarrez/$$i && \
        bash ../codecov-io.sh -f $$file -t $$codecov_token); \
      fi ; \
    done

ifeq ($(BUILDS_SHARED),yes)
install-shared:
endif

$(eval $(call alire_publish,alire.toml,aw/awa,awa-$(VERSION).toml))
$(eval $(call alire_publish,alire-unit.toml,aw/awa_unit,awa_unit-$(VERSION).toml))
