
EXTRA_DIST= model.defs spectrum-type.defs run-g-wrap.scm model-spec.scm model.scm

CLEANFILES=

lib_LTLIBRARIES=libgw-guile-burrowmodel.la
libgw_guile_burrowmodel_la_LDFLAGS=-L.. -lburrow-model @GUILE_GNOME_LIBS@ @GTHREAD_LIBS@ @GUILE_LDFLAGS@ -module
libgw_guile_burrowmodel_la_SOURCES=model-gw.c

preinstalldir=$(abs_top_builddir)/@pre_install_path@
all-local:
	$(INSTALL) -d $(preinstalldir)/share/guile/burrow
	$(INSTALL_DATA) $(srcdir)/model.scm model-gw.scm $(preinstalldir)/share/guile/burrow


headers_dir=@srcdir@/..

HEADERS_1= \
   $(headers_dir)/model.h \
   $(headers_dir)/parameter.h \
   ../model-subtypes-gen.h \
   $(headers_dir)/model-subtypes.h \
   $(headers_dir)/spectrum_model.h \
   $(headers_dir)/solver.h \
   $(headers_dir)/solver_anneal.h

H2DEF=@top_srcdir@/utils/h2def.py

model.defs: $(HEADERS_1) model-spec.scm
	$(H2DEF) --all $(HEADERS_1) > $@

model-gw.c model-gw.h: model.defs
	GUILE_LOAD_PATH=@GUILE_GNOME_MODULE_DIR@:$(GUILE_PATH_GWRAP):@srcdir@:.:$$GUILE_LOAD_PATH \
           $(GUILE) -s @srcdir@/run-g-wrap.scm

INCLUDES=@G_WRAP_CFLAGS@ @GUILE_GNOME_CFLAGS@ @GUILE_CFLAGS@ -I$(headers_dir) -I.. -I@srcdir@/../..

BUILT_SOURCES=model-gw.c model-gw.h model.defs

guiledir=$(datadir)/guile/burrow
guile_DATA=model.scm model-gw.scm

CLEANFILES += model.defs model-gw.c model-gw.h model-gw.scm
CLEANFILES += model.log

