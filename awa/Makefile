## The type of library we want to build. Possible values:
##   relocatable
##   static
AWA_LIBRARY_TYPE=static
HAVE_PANDOC=yes
HAVE_DYNAMO=yes

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XAWA_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XAWA_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XELADA_BUILD=relocatable -XEL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XSECURITYADA_BUILD=relocatable -XSECURITY_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable -XAWS_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTILADA_HTTP_AWS_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTILADA_HTTP_AWS_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_UNIT_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTIL_UNIT_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

PLUGINS=awa-workspaces awa-mail awa-counters awa-storages awa-images awa-comments \
        awa-jobs awa-votes awa-tags awa-flotcharts awa-wikis awa-blogs awa-markedit \
        awa-trumbowyg awa-questions \
	awa-countries awa-settings awa-changelogs awa-setup

NAME=awa
LIBNAME=lib${NAME}

# include rules.mk

DYNAMO_ARGS=--package AWA.Users.Models \
  --package AWA.Events.Models \
  --package AWA.OAuth.Models \
  --package AWA.Permissions.Models \
  --package AWA.Audits.Models \
  db uml/awa.zargo

post-build:
	for i in ${PLUGINS} ; do \
	   $(MAKE) -C plugins/$$i ; \
	done

post-generate:
	for i in ${PLUGINS} ; do \
	   $(MAKE) -C plugins/$$i generate ; \
	done

build-test:: setup
	$(GNATMAKE) -m -p -Pawa_tests $(MAKE_ARGS)

ifeq (${HAVE_PANDOC},yes)
ifeq (${HAVE_DYNAMO},yes)

AWA_DOC= \
  title.md \
  pagebreak.tex \
  index.md \
  pagebreak.tex \
  Installation.md \
  pagebreak.tex \
  Tutorial.md \
  pagebreak.tex \
  AWA.md \
  pagebreak.tex \
  AWA_Users.md \
  pagebreak.tex \
  AWA_Jobs.md \
  pagebreak.tex \
  AWA_Mail.md \
  pagebreak.tex \
  AWA_Storages.md \
  pagebreak.tex \
  AWA_Images.md \
  pagebreak.tex \
  AWA_Wikis.md \
  pagebreak.tex \
  AWA_Blogs.md \
  pagebreak.tex \
  AWA_Counters.md \
  pagebreak.tex \
  AWA_Votes.md \
  pagebreak.tex \
  AWA_Tags.md \
  pagebreak.tex \
  AWA_Comments.md \
  pagebreak.tex \
  AWA_Settings.md \
  pagebreak.tex \
  AWA_Setup.md \
  pagebreak.tex

DOC_OPTIONS=-f markdown -o awa-book.pdf --listings --number-sections --toc --filter ./filter.py
HTML_OPTIONS=-f markdown -o awa-book.html --listings --number-sections --toc --css pandoc.css

docs/awa-book.pdf:  force
	$(DYNAMO) build-doc -pandoc docs
	cd docs && pandoc $(DOC_OPTIONS) --template=./eisvogel.tex $(AWA_DOC)

docs/awa-book.html: docs/awa-book.pdf force
	cd docs && pandoc $(HTML_OPTIONS) $(AWA_DOC)
endif
endif

# Build and run the unit tests
check test:	test-mysql test-sqlite test-postgresql

regtests/result/maildir:
	mkdir -p $@

test-sqlite:		build regtests.db regtests/result/maildir
ifeq (${HAVE_SQLITE},True)
	bin/awa_harness -p SQLite -xml awa-sqlite-aunit.xml -config test-sqlite.properties
endif

test-mysql:		build create-mysql-tests regtests/result/maildir
ifeq (${HAVE_MYSQL},True)
	bin/awa_harness -p MySQL -xml awa-mysql-aunit.xml -config test-mysql.properties
endif

test-postgresql:		build create-postgresql-tests regtests/result/maildir
ifeq (${HAVE_POSTGRESQL},True)
	bin/awa_harness -p Postgresql -xml awa-postgresql-aunit.xml -config test-postgresql.properties
endif

# Create the test sqlite database
regtests.db:
	-rm -f $@
	test T${HAVE_SQLITE} = TTrue && \
	sqlite3 $@ < db/sqlite/create-awa-sqlite.sql

# Create the tables in the database.
# (The database itself must have been created)
create-mysql-tests:
ifeq (${HAVE_MYSQL},True)
	mysql --defaults-extra-file=db/mysql/mysql.cnf < db/mysql/drop-awa-mysql.sql
	mysql --defaults-extra-file=db/mysql/mysql.cnf < db/mysql/create-awa-mysql.sql
endif

# Create the tables in the database.
# (The database itself must have been created)
create-postgresql-tests:
ifeq (${HAVE_POSTGRESQL},True)
	psql -q "postgresql://localhost:5432/awa_test?user=awa_user&password=awa_pass" \
           --file=db/postgresql/drop-awa-postgresql.sql
	psql -q "postgresql://localhost:5432/awa_test?user=awa_user&password=awa_pass" \
           --file=db/postgresql/create-awa-postgresql.sql
endif

# Clean the root project of all build products.
pre-clean:
	-rm -rf obj/* lib/*
	for i in ${PLUGINS} ; do \
	   $(MAKE) -C plugins/$$i clean ; \
	done

pre-install:
	$(MKDIR) -p ${dynamodir}/base/uml
	$(MKDIR) -p ${prefix}/share/doc/awa
ifneq (${GPRINSTALL},)
	$(GPRINSTALL) -p -f --prefix=${prefix} $(MAKE_ARGS) \
		--build-name=$(AWA_LIBRARY_TYPE) awa_unit.gpr
else
	$(MKDIR) -p ${includedir}/awa-unit
	$(MKDIR) -p ${projectdir}/
	${MKDIR} -p ${alidir}/awa-unit/$(AWA_LIBRARY_TYPE)
	${CP} awaunit/*.ad[bs] ${includedir}/awa-unit
	sed -e "s,INSTALL,${prefix},g" -e "s,LIBRARY_TYPE,$(AWA_LIBRARY_TYPE),g" < distrib/awa_unit.gpr > ${projectdir}/awa_unit.gpr
	${CP} lib/awaunit/$(AWA_LIBRARY_TYPE)/*.ali ${alidir}/awa-unit/
ifeq ($(AWA_LIBRARY_TYPE),static)
	${CP} lib/awaunit/$(AWA_LIBRARY_TYPE)/libawa_unit.a ${libdir}/
else
	${CP} -p lib/awaunit/$(AWA_LIBRARY_TYPE)/libawa_unit.$(LIBVER) ${libdir}/
	cd ${libdir} && \
	rm -f libawa_unit.$(LIBEXT) && \
	${LN} libawa_unit.$(LIBVER) libawa_unit.$(LIBEXT)
endif
endif
	for i in ${PLUGINS} ; do \
	   $(MAKE) -C plugins/$$i install; \
	done
	# Install the AWA UML model in Dynamo UML search path
	unzip -cq uml/awa.zargo awa.xmi > ${dynamodir}/base/uml/AWA.xmi

pre-uninstall:
ifneq (${GPRINSTALL},)
	-$(GPRINSTALL) -q -f --prefix=${prefix} $(MAKE_ARGS) --uninstall awa_unit.gpr
else
	rm -rf ${includedir}/awa
	rm -f ${libdir}/libada-awa.a
	rm -f ${projectdir}/awa.gpr
	rm -rf ${dynamodir}/awa
	rm -rf ${alidir}/awa
	rm -rf ${alidir}/awa-unit
	rm -rf ${includedir}/awa-unit
endif
	for i in ${PLUGINS} ; do \
	   $(MAKE) -C plugins/$$i uninstall; \
	done

post-install:	install-info

$(eval $(call ada_library,awa))
$(eval $(call ada_library,awa_unit))

ROOTDIR=.

$(foreach PLUGIN,$(PLUGINS),$(eval include plugins/$(PLUGIN)/Makefile))
