
(define-module (model-spec)
  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap guile)
  #:use-module (gnome gw gobject-spec)
  #:use-module (gnome gw support gobject)
  #:use-module (gnome gw support defs))

(define-class <model-wrapset> (<gobject-wrapset-base>)
  #:id 'model
  #:dependencies '(standard gnome-glib gnome-gobject))

(define-method (global-declarations-cg (self <model-wrapset>))
  (list (next-method)
	"#include <model.h>\n"
	"#include <parameter.h>\n"
	"#include <model-subtypes.h>\n"
	"#include <spectrum_model.h>\n"
	"#include <solver.h>\n"
	"#include <solver_anneal.h>\n"
        "\n"))

(define-method (initializations-cg (self <model-wrapset>) err)
  (list
   "{if (!g_thread_supported ()) g_thread_init (NULL);}"
   (next-method)))

(define-method (initialize (ws <model-wrapset>) initargs)
  (next-method ws (cons #:module (cons '(burrow model) initargs)))
  (load-defs ws "spectrum-type.defs")
  (load-defs ws "model.defs"))

