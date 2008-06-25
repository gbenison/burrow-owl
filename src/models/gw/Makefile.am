
EXTRA_DIST= model.defs
CLEANFILES=

lib_LTLIBRARIES=libgw-guile-model.la
libgw_guile_model_la_LDFLAGS=-L.. -lburrow-model @GUILE_GNOME_LIBS@ @GTHREAD_LIBS@ @GUILE_LDFLAGS@ -module
libgw_guile_model_la_SOURCES=model-gw.c

headers_dir=@srcdir@/..

HEADERS_1= \
   $(headers_dir)/model.h \
   $(headers_dir)/parameter.h \
   ../model-subtypes-gen.h \
   $(headers_dir)/model-subtypes.h \
   $(headers_dir)/spectrum_model.h

H2DEF=@top_srcdir@/utils/h2def.py

model.defs: $(HEADERS_1) model-spec.scm
	$(H2DEF) --all $(HEADERS_1) > $@

model-gw.c model-gw.h: model.defs
	GUILE_LOAD_PATH=@GUILE_GNOME_MODULE_DIR@:$(GUILE_PATH_GWRAP):@srcdir@:.:$$GUILE_LOAD_PATH \
           $(GUILE) -s @srcdir@/run-g-wrap.scm

INCLUDES=@G_WRAP_CFLAGS@ @GUILE_GNOME_CFLAGS@ @GUILE_CFLAGS@ -I$(headers_dir) -I.. -I@srcdir@/../..

BUILT_SOURCES=model-gw.c model-gw.h model.defs

guiledir=$(datadir)/guile/burrow
guile_DATA=model.scm
CLEANFILES += model.scm

model.scm: model-gw.scm
	cp $< $@

CLEANFILES += model.defs model-gw.c model-gw.h model-gw.scm
CLEANFILES += model.log

EXTRA_DIST += run-g-wrap.scm model-spec.scm
