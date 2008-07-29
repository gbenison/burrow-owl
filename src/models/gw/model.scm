
(define-module (burrow model))
(use-modules (oop goops)
	     (gnome gobject)
	     (burrow model-gw))

;; re-export bindings from g-wrap
(module-use! (module-public-interface (current-module))
	     (resolve-interface '(burrow model-gw)))
