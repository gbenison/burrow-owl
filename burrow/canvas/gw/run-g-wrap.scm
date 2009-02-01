#!/bin/sh
# -*-scheme-*-
exec @GUILE@ -s $0 $@
!#

(use-modules (g-wrap)
	     (g-wrap guile)
	     (canvas-spec))

(generate-wrapset 'guile 'burrowcanvas "canvas-gw")
