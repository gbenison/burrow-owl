
check_PROGRAMS=t_spectrum t_integrate t_flake
TESTS = $(check_PROGRAMS) guile-gnome-test run-tests

noinst_LTLIBRARIES=libburrowtest.la
libburrowtest_la_SOURCES=spectrum-ramp.c      spectrum-ramp.h \
                         spectrum-test-cube.c spectrum-test-cube.h \
                         spectrum-flaky.c     spectrum-flaky.h

AM_CPPFLAGS=-I@top_srcdir@/src @GTK_CFLAGS@ @GTHREAD_CFLAGS@
AM_LDFLAGS=-L../src -lburrow -lburrowtest

EXTRA_DIST = tests guile-test guile-gnome-test lib.scm data
DISTCLEANFILES = check-guile.log
