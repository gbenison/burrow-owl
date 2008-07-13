
EXTRA_DIST=
AM_CPPFLAGS=@GTK_CFLAGS@ @GTHREAD_CFLAGS@
SUBDIRS=finite-state-machine . gw models
CLEANFILES=

lib_LTLIBRARIES=libburrow.la

CLEANFILES += $(bin_SCRIPTS)

nobase_data_DATA=guile/spec-extra.scm \
                 guile/burrow/assignments.scm \
                 guile/burrow/hacks.scm \
                 guile/burrow/chemical-shift-distributions.scm \
                 guile/burrow/amino-codes.scm

BUILT_SOURCES=marshal.c marshal.h

burrowincludedir=$(includedir)/burrow
burrowinclude_HEADERS=burrow/spectrum.h
include_HEADERS=burrow.h

# disabled sources -- awaiting updating
# box.c 
# box.h 


libburrow_la_SOURCES=\
@builddir@/marshal.c \
@builddir@/marshal.h \
debug.h \
debug.c \
utils.h \
boomerang.c  \
boomerang.h  \
skiplist.c \
skiplist.h \
burrow.h  \
contour.c  \
contour.h  \
contourplot.c \
contourplot.h \
hosbacking.c  \
hosbacking.h  \
hosbackingsim.c  \
hosbackingsim.h  \
hoscanvas.c  \
hoscanvas.h  \
canvasitem.c \
canvasitem.h \
hosdimension.c  \
hosdimension.h  \
hosdimensionsim.c  \
hosdimensionsim.h  \
spectrum.c  \
burrow/spectrum.h  \
spectrum_priv.h \
spectrum_integrated.c \
spectrum_extract.c \
spectrum_integrated.h \
spectrum_segmented.c \
spectrum_segmented.h \
spectrum_convoluted.c \
spectrum_transpose.c \
spectrum_unfold.c \
spectrum_diagonal_project.c \
spectrum_convoluted.h \
spectrum_project.c \
marker.c  \
marker.h  \
grid.c \
grid.h \
cursor.c \
cursor.h \
line.c \
line.h \
spectrum-profile.c \
spectrum-profile.h \
marker_text.c \
marker_text.h \
nih.c  \
nih.h  \
cairo_shapes.c \
cairo_shapes.h \
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
endian.h \
version.c \
version.h

libburrow_la_LDFLAGS=@GTK_LIBS@ @GTHREAD_LIBS@ -module
libburrow_la_LIBADD=finite-state-machine/contour-fsm.lo

marshal.h: marshal.cmd
	glib-genmarshal --header @srcdir@/marshal.cmd > $@

marshal.c: marshal.cmd
	glib-genmarshal --body @srcdir@/marshal.cmd > $@

CLEANFILES += marshal.c marshal.h
EXTRA_DIST += marshal.cmd guile



