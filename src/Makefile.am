
EXTRA_DIST=
INCLUDES=@GTK_CFLAGS@ @GTHREAD_CFLAGS@
SUBDIRS=gw finite-state-machine
CLEANFILES=

lib_LTLIBRARIES=libgw-guile-spectrum.la
bin_SCRIPTS=burrow

CLEANFILES += $(bin_SCRIPTS)

nobase_data_DATA=guile/spec-extra.scm guile/burrow/assignments.scm

BUILT_SOURCES=marshal.c marshal.h

libgw_guile_spectrum_la_SOURCES=\
@builddir@/marshal.c \
@builddir@/marshal.h \
boomerang.c  \
boomerang.h  \
box.c  \
box.h  \
common.h  \
contour.c  \
contour.h  \
cursor.c  \
cursor.h  \
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
hosdimensionblock.c  \
hosdimensionblock.h  \
hosdimension.c  \
hosdimension.h  \
hosdimensionsim.c  \
hosdimensionsim.h  \
hosspectrum.c  \
hosspectrum.h  \
marker.c  \
marker.h  \
marker_text.c  \
marker_text.h  \
nih.c  \
nih.h  \
ornament.c  \
ornament.h  \
painter_bwps.c  \
painter_bwps.h  \
painter.c  \
painter_gdk.c  \
painter_gdk.h  \
painter.h  \
ticket.c  \
ticket.h \
endian.c \
endian.h

libgw_guile_spectrum_la_LDFLAGS=@GTK_LIBS@ @GTHREAD_LIBS@ @GUILE_GNOME_LIBS@ @GUILE_LDFLAGS@ -module
libgw_guile_spectrum_la_DEPENDENCIES=gw/spectrum-gw.lo
libgw_guile_spectrum_la_LIBADD=gw/spectrum-gw.lo finite-state-machine/contour-fsm.lo

marshal.h: marshal.cmd
	glib-genmarshal --header @srcdir@/marshal.cmd > $@

marshal.c: marshal.cmd
	glib-genmarshal --body @srcdir@/marshal.cmd > $@

CLEANFILES += marshal.c marshal.h
EXTRA_DIST += marshal.cmd guile burrow.in



