
EXTRA_DIST= canvas.defs run-g-wrap.scm canvas-spec.scm canvas.scm
lib_LTLIBRARIES=libgw-guile-burrowcanvas.la
libgw_guile_burrowcanvas_la_LDFLAGS=-L.. -lburrow-canvas @GTK_LIBS@ \
                                    @GTHREAD_LIBS@ @GUILE_GNOME_LIBS@ \
                                    @GUILE_LDFLAGS@ -module
libgw_guile_burrowcanvas_la_SOURCES=canvas-gw.c

headers_dir=@top_srcdir@/src/canvas

preinstalldir=$(abs_top_builddir)/@pre_install_path@
all-local:
	$(INSTALL) -d $(preinstalldir)/share/guile/burrow
	$(INSTALL_DATA) $(srcdir)/canvas.scm canvas-gw.scm $(preinstalldir)/share/guile/burrow


HEADERS_1= \
$(headers_dir)/canvas.h \
$(headers_dir)/canvasitem.h \
$(headers_dir)/ornament.h \
$(headers_dir)/painter_gdk.h \
$(headers_dir)/marker.h \
$(headers_dir)/cursor.h \
$(headers_dir)/grid.h \
$(headers_dir)/line.h \
$(headers_dir)/spectrum-profile.h \
$(headers_dir)/marker_text.h \
$(headers_dir)/contourplot.h
# $(headers_dir)/box.h

H2DEF=@top_srcdir@/utils/h2def.py

canvas.defs: $(HEADERS_1)
	$(H2DEF) --all $(HEADERS_1) > $@

canvas-gw.c canvas-gw.h canvas-gw.scm: canvas.defs
	GUILE_LOAD_PATH=@srcdir@:@builddir@:@GUILE_GNOME_MODULE_DIR@:@G_WRAP_MODULE_DIR@:$$GUILE_LOAD_PATH $(GUILE) -s @srcdir@/run-g-wrap.scm

INCLUDES=@GTK_CFLAGS@ @G_WRAP_CFLAGS@ @GUILE_GNOME_CFLAGS@ @GUILE_CFLAGS@ -I$(headers_dir) -I$(srcdir)/../..

moduledir=$(datadir)/guile/burrow
module_DATA=canvas-gw.scm canvas.scm

CLEANFILES = spectrum.log gnome-atk.log gnome-gdk.log gnome-gtk.log \
           	    gnome-glib.log gnome-pango.log canvas-gw.c canvas-gw.h canvas-gw.scm

