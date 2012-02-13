#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT(burrow-owl, 1.6dev, [gbenison@gmail.com])
AC_PREREQ(2.59)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([burrow/spectrum/spectrum.c])

m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])], [AC_SUBST([AM_DEFAULT_VERBOSITY],1)])

GUILE_PROGS
GUILE_FLAGS

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL

AC_CHECK_PROGS(INDENT, indent, [# ** indent not available **])

dnl Check if doxygen documentation is requested; if so test for presence of doxygen program.
dnl default to program name 'doxygen' if none specified.
AC_ARG_WITH([doxygen],
            [AS_HELP_STRING([--with-doxygen],
                            [build fancy API documentation in html format])],
            [],
            [with_doxygen=doxygen])

dnl Search for requested doxygen program (allow absolute paths)
if test "$with_doxygen" = no; then
  DOXYGEN=["@echo '* doxygen documentation disabled during configuration *'"]
else
  AC_PATH_PROG([DOXYGEN], [$with_doxygen], [not-found], [$PATH:/])
  if test "$DOXYGEN" = "not-found"; then
    AC_MSG_ERROR(
[documentation generator $with_doxygen not found;
 specify location using '--with-doxygen' or disable
 using '--without-doxygen'])
  fi
fi

PKG_CHECK_MODULES(GLIB,      [glib-2.0 gobject-2.0])
PKG_CHECK_MODULES(GDK,       gdk-2.0)
PKG_CHECK_MODULES(GTK,       gtk+-2.0)
PKG_CHECK_MODULES(GTHREAD,   gthread-2.0)
PKG_CHECK_MODULES(STARPARSE, starparse)
PKG_CHECK_MODULES(GUILE_CAIRO, guile-cairo)

GLIB_MKENUMS=`$PKG_CONFIG --variable glib_mkenums glib-2.0`;
GLIB_GENMARSHAL=`$PKG_CONFIG --variable glib_genmarshal glib-2.0`;

AC_SUBST(GLIB_MKENUMS)
AC_SUBST(GLIB_GENMARSHAL)

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
GUILE_GNOME_LIB_DIR=${guile_gnome_prefix}/lib/guile-gnome-$GUILE_GNOME_API;

AC_SUBST(guile_gnome_prefix)
AC_SUBST(GUILE_GNOME_CFLAGS)
AC_SUBST(GUILE_GNOME_LIBS)
AC_SUBST(GUILE_GNOME_MODULE_DIR)
AC_SUBST(GUILE_GNOME_LIB_DIR)

AM_INIT_AUTOMAKE([1.10])

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

# define a location for the pre-installed package.
AC_SUBST([pre_install_path], [pre-install])

AC_CONFIG_FILES([Makefile
                 burrow-owl.pc
                 doc/Makefile
                 doc/Doxyfile
                 wrapper/burrow
		 wrapper/pre-inst-burrow-env
                 burrow/Makefile
		 burrow/spectrum/Makefile
                 burrow/spectrum/gw/Makefile
		 burrow/spectrum/finite-state-machine/Makefile
                 burrow/canvas/Makefile
                 burrow/canvas/gw/Makefile
		 benchmarks/Makefile
		 test-suite/Makefile
		 test-suite/guile-binding/Makefile])

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

