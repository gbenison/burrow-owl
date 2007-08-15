
EXTRA_DIST=
CLEANFILES=
headers_dir=@top_srcdir@/src

lib_LTLIBRARIES=libgw-guile-spectrum.la

libgw_guile_spectrum_la_LDFLAGS=@GTK_LIBS@ @GTHREAD_LIBS@ @GUILE_GNOME_LIBS@ @GUILE_LDFLAGS@ -module
libgw_guile_spectrum_la_SOURCES=spectrum-gw.c
libgw_guile_spectrum_la_LIBADD=../libburrow.la

HEADERS_1= \
$(headers_dir)/burrow.h \
$(headers_dir)/hosspectrum.h \
$(headers_dir)/nih.h \
$(headers_dir)/contour.h \
$(headers_dir)/painter.h \
$(headers_dir)/painter_gdk.h \
$(headers_dir)/painter_bwps.h

HEADERS_2= \
$(headers_dir)/hoscanvas.h \
$(headers_dir)/ornament.h \
$(headers_dir)/hosbackingsim.h \
$(headers_dir)/hosdimensionsim.h

HEADERS_3= \
$(headers_dir)/marker.h \
$(headers_dir)/marker_text.h \
$(headers_dir)/cursor.h \
$(headers_dir)/box.h

H2DEF=@srcdir@/h2def.py

hosspectrum.defs: $(HEADERS_1) $(HEADERS_2) $(HEADERS_3)
	$(H2DEF) --all $(HEADERS_1) > $@
	$(H2DEF) --all $(HEADERS_2) >> $@
	$(H2DEF) --all $(HEADERS_3) >> $@

GGDIR := @top_srcdir@/guile-gnome
GUILE_PATH_GWRAP :=@srcdir@:${G_WRAP_MODULE_DIR}:$(GGDIR)/gtk:$(GGDIR)/gdk:$(GGDIR)/atk:$(GGDIR)/pango:$(GGDIR)/glib:$(GGDIR)/defs:@builddir@

spectrum-gw.c spectrum-gw.h: hosspectrum.defs
	echo $(GUILE_LOAD_PATH)
	GUILE_LOAD_PATH=@GUILE_GNOME_MODULE_DIR@:$(GUILE_PATH_GWRAP):$$GUILE_LOAD_PATH $(GUILE) -s @srcdir@/run-g-wrap.scm

INCLUDES=@GTK_CFLAGS@ @G_WRAP_CFLAGS@ @GUILE_GNOME_CFLAGS@ @GUILE_CFLAGS@ -I$(headers_dir)

## this library is built for the sole purpose of ensuring
## that spectrum-gw.lo gets built.
noinst_LTLIBRARIES=libspec-temp.la
libspec_temp_la_SOURCES=spectrum-gw.c

BUILT_SOURCES=spectrum-gw.c spectrum-gw.h hosspectrum.defs

guiledir=$(datadir)/guile
guile_DATA=spectrum.scm
CLEANFILES += spectrum.scm

spectrum.scm: spectrum-gw.scm
	cp $< $@

CLEANFILES += hosspectrum.defs spectrum-gw.c spectrum-gw.h spectrum-gw.scm
CLEANFILES += spectrum.log gnome-atk.log gnome-gdk.log gnome-gtk.log \
               gnome-glib.log gnome-pango.log

EXTRA_DIST += run-g-wrap.scm h2def.py spectrum-spec.scm



