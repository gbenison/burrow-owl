
SUBDIRS=gw

lib_LTLIBRARIES=libburrow-model.la

BUILT_SOURCES=model-subtypes-gen.h model-subtypes-gen.c

AM_CPPFLAGS=@GLIB_CFLAGS@ @GTHREAD_CFLAGS@

libburrow_model_la_SOURCES=\
model.c \
model.h \
model-subtypes.c \
model-subtypes.h

libburrow_model_la_LDFLAGS=@GLIB_LIBS@ @GTHREAD_LIBS@ -L.. -lburrow -module

model-subtypes-gen.c model-subtypes-gen.h: model-generate.scm
	@GUILE@ -s $<
