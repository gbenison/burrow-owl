
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
  #:dependencies '(standard gnome-glib gnome-gobject gnome-gdk gnome-gtk))

(define-method (global-declarations-cg (self <spectrum-wrapset>))
  (list (next-method)
        "#include <include/spectrum.h>\n"
        "#include <include/spectrum_integrate.h>\n"
        "#include <hoscanvas.h>\n"
        "#include <canvasitem.h>\n"
        "#include <marker.h>\n"
        "#include <marker_text.h>\n"
        "#include <cursor.h>\n"
        "#include <contourplot.h>\n"
        "#include <version.h>\n"
        "#include <contour.h>\n"
	"#include <grid.h>\n"
        "#include <painter.h>\n"
        "#include <painter_bwps.h>\n"
        "#include <painter_gdk.h>\n"
        "#include <include/nih.h>\n"
        "#include <hosbackingsim.h>\n"
        "\n"))

        ; couple of disabled sources
        ; "#include <box.h>\n"
        ; "#include <speciter.h>\n"

#!
(define-method (global-definitions-cg (self <spectrum-wrapset>))
  (list (next-method)))

!#
(define-method (initializations-cg (self <spectrum-wrapset>) err)
  (list
   "{if (!g_thread_supported ()) g_thread_init (NULL);}"
   (next-method)))


(define-method (initialize (ws <spectrum-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(spectrum) initargs)))
  
  (load-defs ws "hosspectrum.defs"))
