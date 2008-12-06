
EXTRA_DIST = guile
SUBDIRS=spectrum canvas
CLEANFILES=

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
	$(INSTALL) -d $(preinstalldir)/share/guile/burrow
	$(INSTALL_DATA) $(srcmoddir)/hacks.scm \
                        $(srcmoddir)/assignments.scm \
                        $(srcmoddir)/chemical-shift-distributions.scm \
                        $(preinstalldir)/share/guile/burrow

clean-local:
	rm -rf $(preinstalldir)

## C api wrappers for burrow-owl header files
burrowincludedir=$(includedir)/burrow
burrowinclude_HEADERS=spectrum.h canvas.h




