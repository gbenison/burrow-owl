
EXTRA_DIST=fsm-compiler contour-spec.fsm contour-fsm.h
INCLUDES=@GLIB_CFLAGS@

contour-fsm.c: contour-spec.fsm
	@srcdir@/fsm-compiler $< >$@
	@INDENT@ $@

BUILT_SOURCES=contour-fsm.c

## this library is built for the sole purpose of ensuring
## that contour-fsm.lo gets built.
noinst_LTLIBRARIES=libfsm-temp.la
libfsm_temp_la_SOURCES=contour-fsm.c
libfsm_temp_la_LDFLAGS=@GLIB_LIBS@







