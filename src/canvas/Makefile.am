
INCLUDES=@GTK_CFLAGS@ @GTHREAD_CFLAGS@
lib_LTLIBRARIES=libburrowcanvas.la

nobase_data_DATA=guile/burrow/canvas.scm

burrowincludedir=$(includedir)/burrow
burrowinclude_HEADERS=burrow/canvas.h

# disabled sources -- awaiting updating
# box.c 
# box.h 

libburrowcanvas_la_SOURCES=\
contourplot.c \
contourplot.h \
hoscanvas.c  \
hoscanvas.h  \
canvasitem.c \
canvasitem.h \
marker.c  \
marker.h  \
grid.c \
grid.h \
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
painter_cairo.h \

libburrowcanvas_la_LDFLAGS=@GTK_LIBS@ @GTHREAD_LIBS@ -module



