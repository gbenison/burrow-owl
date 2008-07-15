
(define-module (spec-extra))

(warn "(use-modules (spec-extra)) is deprecated. It is sooo 2007.  Replaced by (use-modules (burrow spectrum)).")

(module-use! (module-public-interface (current-module))
	     (resolve-interface '(burrow spectrum)))

