
SUBDIRS=. gw

lib_LTLIBRARIES=libburrow-model.la

BUILT_SOURCES=model-subtypes-gen.h model-subtypes-gen.c

AM_CPPFLAGS=@GLIB_CFLAGS@ @GTHREAD_CFLAGS@ -I@srcdir@/.. @GSL_CFLAGS@

libburrow_model_la_SOURCES=\
model.c \
model.h \
spectrum_model.c \
spectrum_model.h \
parameter.c \
parameter.h \
model-subtypes.c \
model-subtypes.h \
solver.c \
solver.h \
solver_anneal.c \
solver_anneal.h

libburrow_model_la_LDFLAGS=@GLIB_LIBS@ @GTHREAD_LIBS@ @GSL_LIBS@ -L.. -lburrow

model-subtypes-gen.c model-subtypes-gen.h: model-generate.scm
	@GUILE@ -s $<
