
check_PROGRAMS=t_spectrum
TESTS = guile-gnome-test run-tests $(check_PROGRAMS)

AM_CPPFLAGS=-I@top_srcdir@/src @GTK_CFLAGS@ @GTHREAD_CFLAGS@
AM_LDFLAGS=-L../src -lburrow

EXTRA_DIST = tests guile-test guile-gnome-test lib.scm data
DISTCLEANFILES = check-guile.log
