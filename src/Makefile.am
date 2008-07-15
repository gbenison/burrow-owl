
EXTRA_DIST=
INCLUDES=@GLIB_CFLAGS@ @GTHREAD_CFLAGS@
SUBDIRS=finite-state-machine . gw canvas
CLEANFILES=

lib_LTLIBRARIES=libburrow.la

CLEANFILES += $(bin_SCRIPTS)

nobase_data_DATA=guile/spec-extra.scm \
                 guile/spectrum.scm \
                 guile/burrow/assignments.scm \
                 guile/burrow/hacks.scm \
                 guile/burrow/chemical-shift-distributions.scm \
                 guile/burrow/amino-codes.scm

BUILT_SOURCES=marshal.c marshal.h

burrowincludedir=$(includedir)/burrow
burrowinclude_HEADERS=burrow/spectrum.h burrow/nih.h
include_HEADERS=burrow.h

libburrow_la_SOURCES=\
@builddir@/marshal.c \
@builddir@/marshal.h \
boomerang.c  \
boomerang.h  \
burrow.h  \
contour.c  \
contour.h  \
filebuf.c  \
filebuf.h  \
hosbackingblock.c  \
hosbackingblock.h  \
hosbacking.c  \
hosbackingcache.c  \
hosbackingcache.h  \
hosbackingfile.c  \
hosbackingfile.h  \
hosbacking.h  \
hosbackingsim.c  \
hosbackingsim.h  \
hosdimensionblock.c  \
hosdimensionblock.h  \
hosdimension.c  \
hosdimension.h  \
hosdimensionsim.c  \
hosdimensionsim.h  \
hosspectrum.c  \
burrow/spectrum.h  \
nih.c  \
burrow/nih.h  \
painter_bwps.c  \
painter_bwps.h  \
painter.c  \
painter.h  \
ticket.c  \
ticket.h \
endian.c \
endian.h \
version.c \
version.h

libburrow_la_LDFLAGS=@GLIB_LIBS@ @GTHREAD_LIBS@ -module
libburrow_la_LIBADD=finite-state-machine/contour-fsm.lo

marshal.h: marshal.cmd
	glib-genmarshal --header @srcdir@/marshal.cmd > $@

marshal.c: marshal.cmd
	glib-genmarshal --body @srcdir@/marshal.cmd > $@

CLEANFILES += marshal.c marshal.h
EXTRA_DIST += marshal.cmd guile



