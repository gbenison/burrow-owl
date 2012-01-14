
(define-module (burrow spectrum))

(use-modules (oop goops)
             (burrow hacks)
	     (gnome gobject)
	     (burrow spectrum-gw))

;; re-export bindings from g-wrap
(module-use! (module-public-interface (current-module))
	     (resolve-interface '(burrow spectrum-gw)))

; -------- utilities ----------

(define-public (write-line x)
  (display x)
  (newline))

(define-public (sleep-if-requested)
  (if (getenv "SLEEP")
      (begin (format #t "pid --> ~a~%" (getpid))
	     (sleep 10)
	     (set! confess
		   (lambda args
		     (apply format (cons #t args))
		     (newline))))))

(define-public confess
  (if (getenv "VERBOSE")
      (lambda args (for-each display args)(newline))
      (lambda args 'ok)))

(define-public (burrow-assert-version major minor)
  (if (not (burrow-check-version major minor))
      (error (format #f "Requested burrow version ~a.~a, but binary version is ~a" major minor (burrow-version)))))

(define-public (command-line-argument n)
  (catch 'out-of-range
	 (lambda ()(list-ref (command-line) n))
	 (lambda args (throw 'usage))))

(define usage-summary "FIXME")
(define usage-arguments '())

(define-public (define-usage str . args)
  (set! usage-summary str)
  (set! usage-arguments args))

(define-public (usage-message . args)
  (let ((prog-name (car (command-line))))
    (format #t "~%~a: ~a~%~%" prog-name usage-summary)
    (format #t "Usage:  ~a  " prog-name)
    (for-each (lambda (arg)(display (car arg))(display "  ")) usage-arguments)
    (newline)
    (newline)))

; ------------- painter commands -------------

(define-public (painter-set-thres painter thres)
  (let ((adj (contour-get-thres-adjustment (painter-get-contour painter))))
    (set adj 'value thres)))

(define-public (painter-set-factor painter factor)
  (let ((adj (contour-get-factor-adjustment (painter-get-contour painter))))
    (set adj 'value factor)))

(define-public (painter-set-nlvl painter x)
  (let ((adj (contour-get-nlvl-adjustment (painter-get-contour painter))))
    (set adj 'value x)))


; ----- manipulations of spectra -------

;; A B A' B' --> (A A') (B B')
(define-public (spectrum-plane-multiply s1 s2)
  (define s #f)
  (set! s (spectrum-convolute s1 s2))      ;; A B A' B'
  (set! s (spectrum-transpose s 2))        ;; A A' B B'
  (set! s (spectrum-diagonal-project s))   ;; (A A') B B'
  (set! s (spectrum-transpose s 2))        ;; B' (A A') B
  (set! s (spectrum-transpose s 2))        ;; B B' (A A')
  (set! s (spectrum-diagonal-project s))   ;; (B B') (A A')
  (set! s (spectrum-transpose s 1))        ;; (A A') (B B')
  s)

;; note: for bruker, dimension order is HNC
;; transform: HNC->CHN->HCN
(define-public (bruker-transpose spec)  ;; HNC
  (set! spec (spectrum-transpose spec 2))  ;; CHN
  (set! spec (spectrum-transpose spec 1))  ;; HCN
  spec)

;
; Sort 'spec' such that dimensions with highest sf come first;
; if sf's are equal, highest np's come first
;
(define-public (spectrum-normalize-order spec)
  (define (dim:> spec a b)
    (cond ((> (spectrum-sf spec a)(spectrum-sf spec b)) #t)
	  ((< (spectrum-sf spec a)(spectrum-sf spec b)) #f)
	  ((> (spectrum-np spec a)(spectrum-np spec b)) #t)
	  (else #f)))
  (define (inversion? spec dim)
    (let loop ((n dim))
      (cond ((< n 0) #f)
	    ((dim:> spec dim n) #t)
	    (else (loop (- n 1))))))
  (let loop ((spec spec)
	     (dim (- (spectrum-ndim spec) 1)))
    (if (= dim 0)
	spec
	(if (inversion? spec dim)
	    (loop (spectrum-transpose spec dim) dim)
	    (loop spec (- dim 1))))))

(define-public (spectrum-extract-2d-ppm spec x1 y1 xn yn)
  (let* ((s1 (spectrum-extract-ppm spec x1 xn))
	 (s2 (spectrum-transpose s1 1))
	 (s3 (spectrum-extract-ppm s2 y1 yn)))
    (spectrum-transpose s3 1)))

; ------------- deperecated & disabled commands -----------

(define-macro (declare-disabled-symbol symbol)
  (if (not (defined? symbol))
      `(define-public ,symbol
	 (lambda args
	   (error
	    (format #f "use of disabled symbol \"~a\" -- is this an old script? burrow version is ~a" (quote ,symbol)(burrow-version)))))))

(declare-disabled-symbol  <sim-peak*>                           )
(declare-disabled-symbol  <sim-profile*>			)
(declare-disabled-symbol  assignment-distance			)
(declare-disabled-symbol  assignment-get-from-list		)
(declare-disabled-symbol  assignment-get-ppm			)
(declare-disabled-symbol  assignment-verified?			)
(declare-disabled-symbol  backing-sim-append-dimension		)
(declare-disabled-symbol  backing-sim-append-peak		)
(declare-disabled-symbol  backing-sim-generate-spectrum		)
(declare-disabled-symbol  backing-sim-new			)
(declare-disabled-symbol  backing-sim-set-pos			)
(declare-disabled-symbol  box-get-x1				)
(declare-disabled-symbol  box-get-xn				)
(declare-disabled-symbol  box-get-y1				)
(declare-disabled-symbol  box-get-yn				)
(declare-disabled-symbol  canvas-add-box			)
(declare-disabled-symbol  canvas-add-ornament			)
(declare-disabled-symbol  canvas-get-painter			)
(declare-disabled-symbol  canvas-get-spectrum			)
(declare-disabled-symbol  canvas-ppm2view			)
(declare-disabled-symbol  canvas-pt2view			)
(declare-disabled-symbol  canvas-set-painter			)
(declare-disabled-symbol  canvas-view2ppm			)
(declare-disabled-symbol  canvas-view2pt			)
(declare-disabled-symbol  contour-get-factor			)
(declare-disabled-symbol  contour-get-factor-adjustment		)
(declare-disabled-symbol  contour-get-levels			)
(declare-disabled-symbol  contour-get-nlvl			)
(declare-disabled-symbol  contour-get-nlvl-adjustment		)
(declare-disabled-symbol  contour-get-thres			)
(declare-disabled-symbol  contour-get-thres-adjustment		)
(declare-disabled-symbol  contour-get-type			)
(declare-disabled-symbol  contour-set-color-negative		)
(declare-disabled-symbol  contour-set-color-positive		)
(declare-disabled-symbol  contour-set-factor-adjustment		)
(declare-disabled-symbol  contour-set-nlvl-adjustment		)
(declare-disabled-symbol  cursor-set-enabled			)
(declare-disabled-symbol  dimension-sim-new			)
(declare-disabled-symbol  file->list				)
(declare-disabled-symbol  hos-backing-sim-get-type		)
(declare-disabled-symbol  hos-box-get-type			)
(declare-disabled-symbol  hos-dimension-sim-get-type		)
(declare-disabled-symbol  hos-painter-gdk-get-type		)
(declare-disabled-symbol  marker-get-pos			)
(declare-disabled-symbol  marker-set-pos			)
(declare-disabled-symbol  ornament-get-group-id			)
(declare-disabled-symbol  ornament-invalidate-region		)
(declare-disabled-symbol  ornament-move				)
(declare-disabled-symbol  ornament-overlap-region		)
(declare-disabled-symbol  ornament-pick-up			)
(declare-disabled-symbol  ornament-redraw			)
(declare-disabled-symbol  ornament-set-group-id			)
(declare-disabled-symbol  ornament-set-region			)
(declare-disabled-symbol  ornament-sync-region			)
(declare-disabled-symbol  ornament-test-grab			)
(declare-disabled-symbol  painter-cancel-redraws		)
(declare-disabled-symbol  painter-gdk-new			)
(declare-disabled-symbol  painter-gdk-set-drawable-gc		)
(declare-disabled-symbol  sim-peak-product-append-profile	)
(declare-disabled-symbol  sim-peak-product-new			)
(declare-disabled-symbol  sim-profile-gaussian-new		)
(declare-disabled-symbol  spectrum-cache			)
(declare-disabled-symbol  spectrum-nih-2d-from-file		)
(declare-disabled-symbol  spectrum-nih-unfold			)
(declare-disabled-symbol  spectrum-project-pt                   )
 


