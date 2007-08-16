#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(burrow, 1.2, [gbenison@gmail.com])
AC_CONFIG_SRCDIR([src/hosspectrum.c])
AC_PREFIX_DEFAULT(${HOME}/AC_PACKAGE_NAME)
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

PKG_CHECK_MODULES(GUILE_GNOME, guile-gnome-glib-0,
{
	AC_MSG_RESULT([Using installed guile-gnome])
	guile_gnome_prefix=`${PKG_CONFIG} --variable=prefix guile-gnome-glib-0`;
	bundle_guile_gnome=no;
	GUILE_GNOME_MODULE_DIR=${guile_gnome_prefix}/share/guile-gnome-0;
	BUNDLE_GUILE_GNOME_DIR="";
},
{
	GUILE_GNOME_MODULE_DIR="";
	if ! test -d ${srcdir}/guile-gnome; then
	  AC_MSG_ERROR([[no installed guile-gnome found, and no bundled guile-gnome present]])
	else
  	  AC_MSG_RESULT([Using bundled guile-gnome])
	  bundle_guile_gnome=yes;
          BUNDLE_GUILE_GNOME_DIR=guile-gnome;	
	  guile_gnome_dir='$(top_srcdir)/guile-gnome';
	  guile_gnome_prefix='${prefix}';
	  GUILE_GNOME_CFLAGS="-I${guile_gnome_dir}/glib -I${guile_gnome_dir}/glib/gnome/gobject";
	  GUILE_GNOME_LIBS='$(top_builddir)/guile-gnome/gtk/gnome/gw/libgw-guile-gnome-gtk.la'
	  AC_CONFIG_SUBDIRS(guile-gnome)
	fi
})

AC_SUBST(guile_gnome_prefix)
AC_SUBST(GUILE_GNOME_CFLAGS)
AC_SUBST(GUILE_GNOME_LIBS)
AC_SUBST(BUNDLE_GUILE_GNOME_DIR)
AC_SUBST(GUILE_GNOME_MODULE_DIR)

AM_INIT_AUTOMAKE
AM_CONDITIONAL(BUNDLE_GUILE_GNOME, test x"$bundle_guile_gnome" = xyes)

# Check for g-wrap
PKG_CHECK_MODULES(G_WRAP, g-wrap-2.0-guile >= 1.9.4,
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
  bundle_g_wrap=no;
},
{

   g_wrap_dir='$(top_srcdir)/guile-gnome/g-wrap';
   g_wrap_builddir='$(top_builddir)/guile-gnome/g-wrap';

   if ! test -d ${srcdir}/guile-gnome/g-wrap; then
     AC_MSG_ERROR([[no installed g-wrap found, and bundled g-wrap not present]])
   else

     AC_MSG_RESULT([Using bundled g-wrap])
     AC_SUBST(G_WRAP_MODULE_DIR, ${g_wrap_dir}:${g_wrap_dir}/lib:${g_wrap_dir}/guile:${g_wrap_builddir}/guile)
     AC_SUBST(G_WRAP_INSTALLED_MODULE_DIR, "")
     AC_SUBST(G_WRAP_CFLAGS, "-I${g_wrap_dir} -I${g_wrap_dir}/libffi/include -I${g_wrap_dir}/guile -I${g_wrap_builddir} -I${g_wrap_builddir}/libffi/include")
     AC_SUBST(G_WRAP_LIBS, "-L${g_wrap_builddir}/libffi -L${g_wrap_builddir}/g-wrap -L${g_wrap_builddir}/guile/g-wrap -lgwrap-guile-runtime -lgwrap-core-runtime -lffi")
     AC_SUBST(G_WRAP_LIB_DIR, ${g_wrap_builddir}/g-wrap:${g_wrap_builddir}/guile/g-wrap:${g_wrap_builddir}/guile/g-wrap/gw)
     bundle_g_wrap=yes;
     AC_SUBST(G_WRAP_INSTALLED_LIB_DIR, "")
   fi
}
)

AM_CONDITIONAL(BUNDLE_G_WRAP, test x"bundle_g_wrap" = xyes)

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
                 src/burrow
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

