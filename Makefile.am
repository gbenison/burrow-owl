
SUBDIRS = burrow test-suite doc benchmarks
bin_SCRIPTS=wrapper/burrow

scriptdir=$(pkgdatadir)/scripts
preinstalldir=$(abs_top_builddir)/@pre_install_path@

all-local:
	$(INSTALL) -d $(preinstalldir)
	$(INSTALL) wrapper/pre-inst-burrow-env $(preinstalldir)

## FIXME perhaps this should be done using more of automake's native
## abilities
script_DATA =
install-data-hook:
	chmod +w $(DESTDIR)$(scriptdir)
	$(INSTALL) $(srcdir)/scripts/* $(DESTDIR)$(scriptdir)

uninstall-hook:
	rm -rf $(DESTDIR)/$(scriptdir)/*

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=burrow-owl.pc

EXTRA_DIST=scripts patches suppress_guile.vg THANKS y2k.txt burrow-owl.mp3 ChangeLog INSTALL-PROBLEMS utils
DISTCLEANFILES=ChangeLog
MAINTAINERCLEANFILES = ChangeLog
.PHONY: ChangeLog
ChangeLog:
	(bzr log $(top_srcdir) > .changelog.tmp && mv .changelog.tmp ChangeLog || rm -rf .changelog.tmp;) || \
	(touch ChangeLog; \
         echo 'failed to generate changelog from bzr: installing possibly empty changelog.' >&2)
dist-hook: ChangeLog




