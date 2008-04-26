#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT(burrow, 1.5rc, [gbenison@gmail.com])
AC_PREREQ(2.59)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/hosspectrum.c])

GUILE_PROGS
GUILE_FLAGS

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL

AC_CHECK_PROGS(INDENT, indent, [# ** indent not available **])

PKG_CHECK_MODULES(GLIB, glib-2.0)
PKG_CHECK_MODULES(GTK, gtk+-2.0)
PKG_CHECK_MODULES(GTHREAD, gthread-2.0)

if test -d $srcdir/nmr-utils; then
   AC_CONFIG_SUBDIRS(nmr-utils)
fi

if test -d $srcdir/starparse; then
   AC_CONFIG_SUBDIRS(starparse)
fi

GUILE_GNOME_API=none;
PKG_CHECK_MODULES(GUILE_GNOME_0, guile-gnome-glib-0 >= 2.15, GUILE_GNOME_API=0, true);
PKG_CHECK_MODULES(GUILE_GNOME_2, guile-gnome-glib-2 >= 2.15, GUILE_GNOME_API=2, true);

if test "$GUILE_GNOME_API" = "none";
then
  AC_MSG_ERROR([No guile-gnome found.])
fi

PKG_CHECK_MODULES(GUILE_GNOME, guile-gnome-glib-$GUILE_GNOME_API >= 2.15);

guile_gnome_prefix=`${PKG_CONFIG} --variable=prefix guile-gnome-glib-$GUILE_GNOME_API`;
GUILE_GNOME_MODULE_DIR=${guile_gnome_prefix}/share/guile-gnome-$GUILE_GNOME_API;

AC_SUBST(guile_gnome_prefix)
AC_SUBST(GUILE_GNOME_CFLAGS)
AC_SUBST(GUILE_GNOME_LIBS)
AC_SUBST(GUILE_GNOME_MODULE_DIR)

AM_INIT_AUTOMAKE

# Check for g-wrap
PKG_CHECK_MODULES(G_WRAP, g-wrap-2.0-guile >= 1.9.8,
{
  AC_MSG_RESULT([Using g-wrap installed in] `${PKG_CONFIG} --prefix g-wrap-2.0-guile`)
  AC_SUBST(G_WRAP_CFLAGS)
  AC_SUBST(G_WRAP_LIBS)
  g_wrap_modules=`${PKG_CONFIG} --variable=module_directory g-wrap-2.0-guile`;
  AC_SUBST(G_WRAP_MODULE_DIR, $g_wrap_modules)
  AC_SUBST(G_WRAP_INSTALLED_MODULE_DIR, $g_wrap_modules)
  g_wrap_libs=`${PKG_CONFIG} --variable=libdir g-wrap-2.0-guile`;
  AC_SUBST(G_WRAP_LIB_DIR, $g_wrap_libs)
  AC_SUBST(G_WRAP_INSTALLED_LIB_DIR, $g_wrap_libs)
})


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([limits.h stdlib.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset pow sqrt])

AC_CONFIG_FILES([Makefile
                 burrow.pc
                 doc/Makefile
                 src/Makefile
                 wrapper/burrow
                 src/gw/Makefile
		 src/finite-state-machine/Makefile
		 test-suite/Makefile])

AC_CONFIG_FILES([test-suite/run-tests], [chmod +x test-suite/run-tests])

AC_OUTPUT

prog_name=burrow;
expanded_bindir=${prefix}/bin;
script_dir=${prefix}/share/${prog_name}/scripts;

AC_MSG_NOTICE([
----------------------------

Configuration complete.
To build the package:

make
make install

The program will be installed as:

$expanded_bindir/burrow

Scripts will be installed in:

$script_dir

To run burrow, either use the full path name:
${expanded_bindir}/burrow
or place ${expanded_bindir} in your PATH.

To install in a different directory, try:

./configure --prefix=/path/to/some/directory

----------------------------]
)

