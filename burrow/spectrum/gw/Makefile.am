
EXTRA_DIST= hosspectrum.defs
CLEANFILES=
headers_dir=$(srcdir)/..

lib_LTLIBRARIES=libgw-guile-spectrum.la

preinstalldir=$(abs_top_builddir)/@pre_install_path@
all-local:
	$(INSTALL) -d $(preinstalldir)/share/guile/burrow
	$(INSTALL_DATA) $(srcdir)/spectrum.scm spectrum-gw.scm $(preinstalldir)/share/guile/burrow

libgw_guile_spectrum_la_LDFLAGS=@GLIB_LIBS@ @GTHREAD_LIBS@ \
                                @GUILE_GNOME_LIBS@ \
                                @GUILE_LDFLAGS@ -L.. -lburrow -module
libgw_guile_spectrum_la_SOURCES=spectrum-gw.c

HEADERS_1=                                     \
$(headers_dir)/spectrum.h             	       \
$(headers_dir)/contour.h		       \
$(headers_dir)/nih.h			       \
$(headers_dir)/painter.h		       \
$(headers_dir)/painter_bwps.h		       \
$(headers_dir)/spectrum_convoluted.h	       \
$(headers_dir)/spectrum_diagonal_project.h     \
$(headers_dir)/spectrum_extract.h	       \
$(headers_dir)/spectrum_integrated.h	       \
$(headers_dir)/spectrum_project.h	       \
$(headers_dir)/spectrum_transpose.h	       \
$(headers_dir)/spectrum_unfold.h	       \
$(headers_dir)/version.h                      

H2DEF=@top_srcdir@/utils/h2def.py

hosspectrum.defs: $(HEADERS_1)
	$(H2DEF) --all $(HEADERS_1) > $@

spectrum-gw.c spectrum-gw.h: hosspectrum.defs
	GUILE_LOAD_PATH=@srcdir@:@builddir@:@GUILE_GNOME_MODULE_DIR@:@G_WRAP_MODULE_DIR@:$$GUILE_LOAD_PATH $(GUILE) -s @srcdir@/run-g-wrap.scm

INCLUDES=@GLIB_CFLAGS@ @G_WRAP_CFLAGS@ @GUILE_GNOME_CFLAGS@ @GUILE_CFLAGS@ -I@top_srcdir@

BUILT_SOURCES=spectrum-gw.c spectrum-gw.h hosspectrum.defs

moduledir=$(datadir)/guile/burrow
module_DATA=spectrum-gw.scm spectrum.scm

CLEANFILES += hosspectrum.defs spectrum-gw.c spectrum-gw.h spectrum-gw.scm
CLEANFILES += spectrum.log gnome-glib.log

EXTRA_DIST += run-g-wrap.scm spectrum-spec.scm spectrum.scm



