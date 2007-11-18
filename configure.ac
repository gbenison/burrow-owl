#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(burrow, 1.3, [gbenison@gmail.com])
AC_CONFIG_SRCDIR([src/hosspectrum.c])
# AC_CONFIG_HEADER([config.h])

guile-config --version || AC_MSG_ERROR([

--------------------
Configuration failed; is guile
(with headers / development package) installed?
--------------------
 
])

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL

PKG_CHECK_MODULES(GLIB, glib-2.0)
PKG_CHECK_MODULES(GTK, gtk+-2.0)
PKG_CHECK_MODULES(GTHREAD, gthread-2.0)

# FIXME use pkg-config --variable=prefix to
# find installed library location; then use this to set
# path in guile-conv

AC_CONFIG_SUBDIRS(nmr-utils)

PKG_CHECK_MODULES(GUILE_GNOME, guile-gnome-glib-0 >= 2.15,
{
	AC_MSG_RESULT([Using installed guile-gnome])
	guile_gnome_prefix=`${PKG_CONFIG} --variable=prefix guile-gnome-glib-0`;
	GUILE_GNOME_MODULE_DIR=${guile_gnome_prefix}/share/guile-gnome-0;
})

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

GUILE_PROGS
GUILE_FLAGS

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

