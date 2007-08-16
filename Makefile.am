

SUBDIRS = nmr-utils @BUNDLE_GUILE_GNOME_DIR@ src test-suite doc

## if BUNDLE_GUILE_GNOME
## SUBDIRS += guile-gnome
## endif
## SUBDIRS += src test-suite

scriptdir=$(pkgdatadir)/scripts

## FIXME perhaps this should be done using more of automake's native
## abilities
script_DATA =
install-data-hook:
	chmod +w $(scriptdir)
	$(INSTALL) $(srcdir)/scripts/* $(scriptdir)

uninstall-hook:
	rm -rf $(scriptdir)/*

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=burrow.pc

EXTRA_DIST=scripts suppress_guile.vg THANKS burrow-owl.mp3

dist-hook:
	rm -rf `find $(distdir) -name CVS`
	rm -rf `find $(distdir) -name '.arch*'`

