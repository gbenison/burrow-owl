
headers_dir=@top_srcdir@/src/canvas

HEADERS= \
$(headers_dir)/hoscanvas.h \
$(headers_dir)/canvasitem.h \
$(headers_dir)/ornament.h \
$(headers_dir)/painter_gdk.h \
$(headers_dir)/marker.h \
$(headers_dir)/cursor.h \
$(headers_dir)/grid.h \
$(headers_dir)/marker_text.h \
$(headers_dir)/contourplot.h
# $(headers_dir)/box.h

INCLUDES=@GTK_CFLAGS@ @G_WRAP_CFLAGS@ @GUILE_GNOME_CFLAGS@ @GUILE_CFLAGS@ -I$(headers_dir)

CLEANFILES += spectrum.log gnome-atk.log gnome-gdk.log gnome-gtk.log \
               gnome-glib.log gnome-pango.log

