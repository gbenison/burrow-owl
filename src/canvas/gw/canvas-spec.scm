
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
        "#include \"spectrum.h\"\n"
        "#include \"canvas.h\"\n"
        "#include \"canvasitem.h\"\n"
        "#include \"marker.h\"\n"
        "#include \"marker_text.h\"\n"
        "#include \"cursor.h\"\n"
        "#include \"contourplot.h\"\n"
	"#include \"grid.h\"\n"
	"#include \"line.h\"\n"
	"#include \"spectrum-profile.h\"\n"
        "#include \"painter_gdk.h\"\n"
        "\n"))

        ; couple of disabled sources
        ; "#include \"box.h\"\n"
        ; "#include \"speciter.h\"\n"

(define-method (initializations-cg (self <canvas-wrapset>) err)
  (list
   "{if (!g_thread_supported ()) g_thread_init (NULL);}"
   (next-method)))

(define-method (initialize (ws <canvas-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(burrow canvas-gw) initargs)))
  (load-defs ws "canvas.defs"))
