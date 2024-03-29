
(define-module (canvas-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw gdk-spec)
  #:use-module (gnome gw gtk-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <canvas-wrapset> (<gobject-wrapset-base>)
  #:id 'burrowcanvas
  #:dependencies '(standard gnome-glib gnome-gobject gnome-gdk gnome-gtk))

(define-method (global-declarations-cg (self <canvas-wrapset>))
  (list (next-method)
        "#include \"burrow/spectrum.h\"\n"
        "#include \"burrow/canvas.h\"\n"
	"#include \"guile-cairo.h\"\n"
        "\n"))

(define-method (initializations-cg (self <canvas-wrapset>) err)
  (list
   "{if (!g_thread_supported ()) g_thread_init (NULL);}"
   (next-method)))

(define-method (initialize (ws <canvas-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(burrow canvas-gw) initargs)))
  (load-defs ws "canvas.defs"))
