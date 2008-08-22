
EXTRA_DIST=
AM_CPPFLAGS=@GLIB_CFLAGS@ @GTHREAD_CFLAGS@
SUBDIRS=finite-state-machine . gw canvas models
CLEANFILES=

lib_LTLIBRARIES=libburrow.la

CLEANFILES += $(bin_SCRIPTS)

nobase_data_DATA=guile/spec-extra.scm \
                 guile/spectrum.scm \
                 guile/burrow/assignments.scm \
                 guile/burrow/hacks.scm \
                 guile/burrow/chemical-shift-distributions.scm \
                 guile/burrow/amino-codes.scm

## copy modules to pre-install directory
srcmoddir=$(srcdir)/guile/burrow
preinstalldir=$(abs_top_builddir)/@pre_install_path@
all-local:
        ## test automake behavior.
	## older versions of automake appear not to set abs_top_builddir.
	@test -f $(abs_top_builddir)/burrow-owl.pc || { echo "!!!!!!!!!!!!!!! CHECK AUTOMAKE VERSION >= 1.10 !!!!!!!!!!!!!!" && exit 1; }
	$(INSTALL) -d $(preinstalldir)/share/guile/burrow
	$(INSTALL_DATA) $(srcmoddir)/hacks.scm $(srcmoddir)/assignments.scm $(preinstalldir)/share/guile/burrow

BUILT_SOURCES=marshal.c marshal.h

burrowincludedir=$(includedir)/burrow
burrowinclude_HEADERS=burrow/spectrum.h
include_HEADERS=burrow.h

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
nih.c  \
nih.h  \
painter_bwps.c  \
painter_bwps.h  \
painter.c  \
painter.h  \
ticket.c  \
ticket.h \
endian.c \
endian.h \
version.c \
version.h

libburrow_la_LDFLAGS=@GLIB_LIBS@ @GTHREAD_LIBS@ -lm
libburrow_la_LIBADD=finite-state-machine/contour-fsm.lo

marshal.h: marshal.cmd
	glib-genmarshal --header @srcdir@/marshal.cmd > $@

marshal.c: marshal.cmd
	glib-genmarshal --body @srcdir@/marshal.cmd > $@

CLEANFILES += marshal.c marshal.h

clean-local:
	rm -rf $(preinstalldir)

EXTRA_DIST += marshal.cmd guile



