
check_PROGRAMS=t_spectrum t_integrate t_flake t_convolute t_async \
               t_skiplist t_segment_simple t_segment   \
               t_segment_async t_project     \
               t_diagonal_project t_extract t_transpose t_unfold  \
               t_cache \
               t_collision
TESTS = $(check_PROGRAMS) guile-gnome-test run-tests

noinst_LTLIBRARIES=libburrowtest.la
libburrowtest_la_SOURCES=spectrum-ramp.c      spectrum-ramp.h \
                         spectrum-test-cube.c spectrum-test-cube.h \
                         spectrum-flaky.c     spectrum-flaky.h \
                         segment-sim.c        segment-sim.h

AM_CPPFLAGS=-I@top_srcdir@/src @GTK_CFLAGS@ @GTHREAD_CFLAGS@
AM_LDFLAGS=-L../src -lburrow -lburrowtest

EXTRA_DIST = tests guile-test guile-gnome-test lib.scm data
DISTCLEANFILES = check-guile.log
