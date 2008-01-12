
(define-module (nmr amino-codes)
  #:use-module (srfi srfi-1)
  #:export (aa:three2one
	    aa:one2three
	    aa:three2name
	    aa:one2name))

(define amino-codes
  '(("alanine"                              ALA A )
    ("arginine"                             ARG R )
    ("asparagine"                           ASN N )
    ("aspartic acid"                        ASP D )
    ("asparagine or aspartic acid"          ASX B )
    ("cysteine"                             CYS C )
    ("glutamic acid"                        GLU E )
    ("glutamine"                            GLN Q )
    ("glutamine or glutamic acid"           GLX Z )
    ("glycine"                              GLY G )
    ("histidine"                            HIS H )
    ("isoleucine"                           ILE I )
    ("leucine"                              LEU L )
    ("lysine"                               LYS K )
    ("methionine"                           MET M )
    ("phenylalanine"                        PHE F )
    ("proline"                              PRO P )
    ("serine"                               SER S )
    ("threonine"                            THR T )
    ("tryptophan"                           TRP W )
    ("tyrosine"                             TYR Y )
    ("valine"                               VAL V )))

(define aa-name car)
(define aa-three cadr)
(define aa-one caddr)

(define (normalize-symbol x)
  (string->symbol
   (string-upcase
    (format #f "~a" x))))

(define (lookup/one code)
  (find (lambda (aa)
	  (equal? (aa-one aa)
		  (normalize-symbol code))) amino-codes))

(define (lookup/three code)
  (find (lambda (aa)
	  (equal? (aa-three aa)
		  (normalize-symbol code))) amino-codes))

(define (aa:one2three code)
  (false-if-exception
   (aa-three (lookup/one code))))

(define (aa:three2one code)
  (false-if-exception
   (aa-one (lookup/three code))))

(define (aa:three2name code)
  (false-if-exception
   (aa-name (lookup/three code))))

(define (aa:one2name code)
  (false-if-exception
   (aa-name (lookup/one code))))
