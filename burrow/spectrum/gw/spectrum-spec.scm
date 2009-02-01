
(define-module (spectrum-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw gdk-spec)
  #:use-module (gnome gw gtk-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <spectrum-wrapset> (<gobject-wrapset-base>)
  #:id 'spectrum
  #:dependencies '(standard gnome-glib gnome-gobject))

(define-method (global-declarations-cg (self <spectrum-wrapset>))
  (list (next-method)
        "#include \"burrow/spectrum.h\"\n"
        "\n"))

#!
(define-method (global-definitions-cg (self <spectrum-wrapset>))
  (list (next-method)))
!#

(define-method (initializations-cg (self <spectrum-wrapset>) err)
  (list
   "{if (!g_thread_supported ()) g_thread_init (NULL);}"
   (next-method)))

(define-method (initialize (ws <spectrum-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(burrow spectrum-gw) initargs)))
  (load-defs ws "hosspectrum.defs"))
