
INCLUDES=@GTK_CFLAGS@ @GTHREAD_CFLAGS@ -I$(srcdir)/..
lib_LTLIBRARIES=libburrow-canvas.la
CLEANFILES=
EXTRA_DIST=
SUBDIRS=. gw

## burrowincludedir=$(includedir)/burrow/canvas
## burrowinclude_HEADERS=canvas.h

BUILT_SOURCES=marshal.c marshal.h

# disabled sources -- awaiting updating
# box.c 
# box.h 

libburrow_canvas_la_SOURCES=\
@builddir@/marshal.c \
@builddir@/marshal.h \
contourplot.c \
contourplot.h \
canvas.c  \
canvas.h  \
canvasitem.c \
canvasitem.h \
marker.c  \
marker.h  \
grid.c \
grid.h \
line.c \
line.h \
spectrum-profile.c \
spectrum-profile.h \
cursor.c \
cursor.h \
marker_text.c \
marker_text.h \
cairo_shapes.c \
cairo_shapes.h \
ornament.c  \
ornament.h  \
painter_gdk.c  \
painter_gdk.h  \
painter_cairo.c \
painter_cairo.h

libburrow_canvas_la_LDFLAGS=@GTK_LIBS@ @GTHREAD_LIBS@ -L.. -lburrow

marshal.h: marshal.cmd
	glib-genmarshal --header @srcdir@/marshal.cmd > $@

marshal.c: marshal.cmd
	glib-genmarshal --body @srcdir@/marshal.cmd > $@

CLEANFILES += marshal.c marshal.h
EXTRA_DIST += marshal.cmd


