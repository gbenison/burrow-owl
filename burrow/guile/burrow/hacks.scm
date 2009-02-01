
(define-module (burrow hacks))

;
; This module contains workarounds for dependency bugs...
; it is not supposed to contain functionality.
;

(define (fake-connect . args)
  "This is just a place-holder 'connect' function placed into the root module
   by burrow-owl, to work around a bug that prevents guile-gnome (<= version 2.15.95)
   from loading in guiles that lack a native 'connect' function.
")

(if (not (module-defined? the-root-module 'connect))
    (module-define! the-root-module 'connect fake-connect))

