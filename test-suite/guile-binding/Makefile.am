
TESTS_ENVIRONMENT=EXAMPLE_DIR=@srcdir@/../data \
                  G_SLICE=debug-blocks \
                  guile=@GUILE@ \
                  @top_builddir@/@pre_install_path@/pre-inst-burrow-env

TESTS=guile-gnome.test    \
      gdk-color.test      \
      open-spectrum.test  \
      transpose.test      \
      diagonal-projection.test \
      regression-1.2.test \
      spectrum-gc.test    \
      assignment-convert.test \
      no-display.test \
      no-display-fail.test

XFAIL_TESTS=no-display-fail.test

EXTRA_DIST=$(TESTS)
