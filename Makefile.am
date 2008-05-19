
SUBDIRS = @subdirs@ src test-suite doc benchmarks
bin_SCRIPTS=wrapper/burrow

scriptdir=$(pkgdatadir)/scripts

## FIXME perhaps this should be done using more of automake's native
## abilities
script_DATA =
install-data-hook:
	chmod +w $(DESTDIR)$(scriptdir)
	$(INSTALL) $(srcdir)/scripts/* $(DESTDIR)$(scriptdir)

uninstall-hook:
	rm -rf $(DESTDIR)/$(scriptdir)/*

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=burrow.pc

EXTRA_DIST=scripts suppress_guile.vg THANKS y2k.txt burrow-owl.mp3 srfi-35.diff ChangeLog INSTALL-PROBLEMS
DISTCLEANFILES=ChangeLog
MAINTAINERCLEANFILES = ChangeLog
.PHONY: ChangeLog
ChangeLog:
	(bzr log $(top_srcdir) > .changelog.tmp && mv .changelog.tmp ChangeLog || rm -rf .changelog.tmp;) || \
	(touch ChangeLog; \
         echo 'failed to generate changelog from bzr: installing possibly empty changelog.' >&2)
dist-hook: ChangeLog




