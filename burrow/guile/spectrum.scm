
(define-module (spectrum))

(warn "(use-modules (spectrum)) is deprecated. It is sooo 2007.  Replaced by (use-modules (burrow spectrum)).")

(module-use! (module-public-interface (current-module))
	     (resolve-interface '(burrow spectrum)))

