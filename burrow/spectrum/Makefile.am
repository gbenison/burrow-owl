
AM_CPPFLAGS=@GLIB_CFLAGS@ @GTHREAD_CFLAGS@
SUBDIRS=finite-state-machine . gw

lib_LTLIBRARIES=libburrow.la

BUILT_SOURCES=marshal.c marshal.h

burrow_c_built_sources=        \
marshal.c

burrow_c_sources=              \
debug.c 		       \
skiplist.c 		       \
contour.c  		       \
spectrum.c  		       \
spectrum_integrated.c 	       \
spectrum_extract.c 	       \
spectrum_segmented.c 	       \
spectrum_convoluted.c 	       \
spectrum_transpose.c 	       \
spectrum_unfold.c 	       \
spectrum_diagonal_project.c    \
spectrum_project.c 	       \
nih.c  			       \
painter_bwps.c  	       \
painter.c  		       \
ticket.c  		       \
burrow_endian.c 	       \
version.c                      \
$(burrow_c_built_sources)

burrow_private_built_headers=  \
marshal.h

burrow_private_headers=        \
debug.h 		       \
utils.h 		       \
skiplist.h 		       \
spectrum_priv.h 	       \
painter_bwps.h  	       \
painter.h  		       \
burrow_endian.h		       \
$(burrow_private_built_headers)

# headers included from <burrow/spectrum.h>
burrow_public_headers=         \
spectrum.h             	       \
contour.h		       \
nih.h			       \
painter.h		       \
painter_bwps.h		       \
spectrum.h		       \
spectrum_segmented.h 	       \
spectrum_convoluted.h	       \
spectrum_diagonal_project.h    \
spectrum_extract.h	       \
spectrum_integrated.h	       \
spectrum_project.h	       \
spectrum_transpose.h	       \
spectrum_unfold.h	       \
version.h                      

libburrow_la_SOURCES=$(burrow_c_sources) $(burrow_c_built_sources) \
                     $(burrow_public_headers) $(burrow_private_headers)

libburrow_la_LDFLAGS=@GLIB_LIBS@ @GTHREAD_LIBS@ -lm
libburrow_la_LIBADD=finite-state-machine/contour-fsm.lo

marshal.h: marshal.cmd
	glib-genmarshal --header @srcdir@/marshal.cmd > $@

marshal.c: marshal.cmd
	glib-genmarshal --body @srcdir@/marshal.cmd > $@

CLEANFILES = marshal.c marshal.h
EXTRA_DIST = marshal.cmd

# Installed C API headers
burrowincludedir=$(includedir)/burrow/spectrum
burrowinclude_HEADERS=$(burrow_public_headers)
