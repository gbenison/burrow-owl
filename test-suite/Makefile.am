
check_PROGRAMS=t_spectrum t_integrate t_flake t_convolute t_async \
               t_skiplist t_segment_simple t_segment   \
               t_segment_async t_project     \
               t_diagonal_project t_extract t_transpose t_unfold  \
               t_painter_redraw \
               t_cache \
               t_collision \
               t_traverse_blocking \
               t_cancel \
               t_traverse_override
TESTS_ENVIRONMENT=EXAMPLE_DIR=@srcdir@/data
TESTS = $(check_PROGRAMS)
SUBDIRS=. guile-binding

noinst_LTLIBRARIES=libburrowtest.la
libburrowtest_la_SOURCES=spectrum-ramp.c      spectrum-ramp.h \
                         spectrum-test-cube.c spectrum-test-cube.h \
                         spectrum-flaky.c     spectrum-flaky.h \
                         segment-sim.c        segment-sim.h \
                         test-utils.c         test-utils.h

AM_CPPFLAGS=-I@top_srcdir@ -I@top_srcdir@/burrow/spectrum @GTK_CFLAGS@ @GTHREAD_CFLAGS@
AM_LDFLAGS=-L../burrow/spectrum -lburrow -lburrowtest

EXTRA_DIST =  data
DISTCLEANFILES = check-guile.log


