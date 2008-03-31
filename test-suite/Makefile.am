
check_PROGRAMS=t_spectrum t_integrate
TESTS = $(check_PROGRAMS) guile-gnome-test run-tests

t_spectrum_SOURCES=t_spectrum.c spectrum-ramp.c spectrum-ramp.h
t_integrate_SOURCES=t_integrate.c spectrum-ramp.c spectrum-ramp.h

AM_CPPFLAGS=-I@top_srcdir@/src @GTK_CFLAGS@ @GTHREAD_CFLAGS@
AM_LDFLAGS=-L../src -lburrow

EXTRA_DIST = tests guile-test guile-gnome-test lib.scm data
DISTCLEANFILES = check-guile.log
