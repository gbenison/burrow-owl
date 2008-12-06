#!/bin/sh
# -*-scheme-*-
exec @GUILE@ -s $0 $@
!#

(use-modules (g-wrap)
	     (g-wrap guile)
	     (spectrum-spec))

(generate-wrapset 'guile 'spectrum "spectrum-gw")


