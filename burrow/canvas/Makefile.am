
INCLUDES=@GTK_CFLAGS@ @GTHREAD_CFLAGS@ -I@top_srcdir@
lib_LTLIBRARIES=libburrow-canvas.la
CLEANFILES=
EXTRA_DIST=
SUBDIRS=. gw

# disabled sources -- awaiting updating
# box.c 
# box.h 

canvas_c_built_sources= marshal.c

canvas_c_sources=        \
contourplot.c 		 \
boomerang.c 		 \
canvas.c  		 \
canvasitem.c 		 \
marker.c  		 \
grid.c 			 \
line.c 			 \
spectrum-profile.c 	 \
cursor.c 		 \
marker_text.c 		 \
cairo_shapes.c 		 \
ornament.c  		 \
painter_gdk.c  		 \
painter_cairo.c 	 

canvas_private_built_headers= marshal.h

canvas_private_headers=  \
boomerang.h 		 \
cairo_shapes.h 		 \
painter_gdk.h  		 \
painter_cairo.h          \
$(canvas_private_built_headers)

canvas_public_headers=   \
canvas.h		 \
canvasitem.h		 \
contourplot.h		 \
cursor.h		 \
grid.h			 \
line.h			 \
marker.h		 \
marker_text.h		 \
ornament.h		 \
spectrum-profile.h       

BUILT_SOURCES=$(canvas_c_built_sources) $(canvas_private_built_headers)

libburrow_canvas_la_SOURCES=$(canvas_c_sources) $(canvas_c_built_sources) \
                            $(canvas_public_headers) $(canvas_private_headers)

libburrow_canvas_la_LDFLAGS=@GTK_LIBS@ @GTHREAD_LIBS@ -L../spectrum -lburrow

marshal.h: marshal.cmd
	glib-genmarshal --header @srcdir@/marshal.cmd > $@

marshal.c: marshal.cmd
	glib-genmarshal --body @srcdir@/marshal.cmd > $@

CLEANFILES += marshal.c marshal.h
EXTRA_DIST += marshal.cmd

burrowincludedir=$(includedir)/burrow/canvas
burrowinclude_HEADERS=$(canvas_public_headers)
