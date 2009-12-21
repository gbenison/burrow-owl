
INCLUDES=@GTK_CFLAGS@ @GTHREAD_CFLAGS@ -I@top_srcdir@
lib_LTLIBRARIES=libburrow-canvas.la
CLEANFILES=
EXTRA_DIST=
SUBDIRS=. gw

# disabled sources -- awaiting updating
# box.c 
# box.h 

canvas_c_built_sources= marshal.c canvas-enums.c

canvas_c_sources=        \
contour-color.c          \
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
contour-color.h          \
canvas.h		 \
canvasitem.h		 \
canvas-enums.h           \
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
	( cd @srcdir@ && $(GLIB_GENMARSHAL) \
	 --header marshal.cmd > xgen-marshal \
	&& cp xgen-marshal $@ \
	&& rm -f xgen-marshal

marshal.c: marshal.cmd
	( cd @srcdir@ && $(GLIB_GENMARSHAL) \
	--body marshal.cmd > xgen-marshal \
	&& cp xgen-marshal $@ \
	&& rm -f xgen-marshal

enum_headers=canvas-enums.h

canvas-enums.c: canvas-enums.template $(enum_headers)
	( cd @srcdir@ && $(GLIB_MKENUMS) \
		 --template canvas-enums.template \
                 $(enum_headers) ) > xgen-enums \
	&& cp xgen-enums $@ \
	&& rm -f xgen-enums

CLEANFILES += $(canvas_c_built_sources) $(canvas_private_built_headers)
EXTRA_DIST += marshal.cmd canvas-enums.template

burrowincludedir=$(includedir)/burrow/canvas
burrowinclude_HEADERS=$(canvas_public_headers)
