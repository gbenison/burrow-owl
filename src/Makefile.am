
EXTRA_DIST=
INCLUDES=@GTK_CFLAGS@ @GTHREAD_CFLAGS@
SUBDIRS=finite-state-machine . gw 
CLEANFILES=

lib_LTLIBRARIES=libburrow.la

CLEANFILES += $(bin_SCRIPTS)

nobase_data_DATA=guile/spec-extra.scm guile/burrow/assignments.scm

BUILT_SOURCES=marshal.c marshal.h

burrowincludedir=$(includedir)/burrow
burrowinclude_HEADERS=burrow/spectrum.h burrow/nih.h
include_HEADERS=burrow.h

# disabled sources -- awaiting updating
# box.c 
# box.h 
# cursor.c
# cursor.h
# marker_text.c
# marker_text.h

libburrow_la_SOURCES=\
@builddir@/marshal.c \
@builddir@/marshal.h \
boomerang.c  \
boomerang.h  \
burrow.h  \
contour.c  \
contour.h  \
contourplot.c \
contourplot.h \
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
hoscanvas.c  \
hoscanvas.h  \
canvasitem.c \
canvasitem.h \
hosdimensionblock.c  \
hosdimensionblock.h  \
hosdimension.c  \
hosdimension.h  \
hosdimensionsim.c  \
hosdimensionsim.h  \
hosspectrum.c  \
burrow/spectrum.h  \
marker.c  \
marker.h  \
nih.c  \
burrow/nih.h  \
ornament.c  \
ornament.h  \
painter_bwps.c  \
painter_bwps.h  \
painter_gdk.c  \
painter_gdk.h  \
painter_cairo.c \
painter_cairo.h \
painter.c  \
painter.h  \
ticket.c  \
ticket.h \
endian.c \
endian.h

libburrow_la_LDFLAGS=@GTK_LIBS@ @GTHREAD_LIBS@ -module
libburrow_la_LIBADD=finite-state-machine/contour-fsm.lo

marshal.h: marshal.cmd
	glib-genmarshal --header @srcdir@/marshal.cmd > $@

marshal.c: marshal.cmd
	glib-genmarshal --body @srcdir@/marshal.cmd > $@

CLEANFILES += marshal.c marshal.h
EXTRA_DIST += marshal.cmd guile



