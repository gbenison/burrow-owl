;; Some extra utilities
;; useful with convoluted spectra.

(define-module (spec-extra))

(use-modules (spectrum))

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


; ------------- canvas commands -------------

(define-public (canvas-set-spectrum canv spec)
  (painter-set-spectrum (canvas-get-painter canv) spec))

(define-public (canvas-set-thres canv thres)
  (contour-set-thres-adjustment
   (painter-get-contour (canvas-get-painter canv)) thres))

(define-public (canvas-set-draw-negative canv neg?)
  (contour-set-draw-negative
   (painter-get-contour
    (canvas-get-painter canv)) neg?))

(define-public (painter-set-thres painter thres)
  (let ((adj (contour-get-thres-adjustment (painter-get-contour painter))))
    (set adj 'value thres)))

(define-public (painter-set-factor painter factor)
  (let ((adj (contour-get-factor-adjustment (painter-get-contour painter))))
    (set adj 'value factor)))

(define-public (painter-set-nlvl painter x)
  (let ((adj (contour-get-nlvl-adjustment (painter-get-contour painter))))
    (set adj 'value x)))

; ---- Color schemes for gdk painters ----
(define-public (set-to-purple painter)
  (contour-set-color-positive (painter-get-contour painter)
			      5000 65000 0 0 40000 60000))

(define-public (set-to-greenish painter)
  (contour-set-color-positive (painter-get-contour painter)
			      0 0 5000 65000 40000 60000))
(define-public (set-to-reddish painter)
  (contour-set-color-positive (painter-get-contour painter)
			      30000 60000 5000 40000 0 0))

; ------ file handling -----
(define-public (file->list fname)
  (let ((f (open-input-file fname)))
    (define (grok result)
      (let ((next (read f)))
	(if (eof-object? next)
	    (reverse result)
	    (grok (cons next result)))))
    (and f
	 (grok (list)))))

; ------- assignments -------
; accept two styles of assignment list
(define-public (assignment-get assignments residue atom)
  (let ((entry (assoc residue assignments)))
    (and entry
	 (let ((asgs (or (false-if-exception (cdr (assoc 'assignments (cdr entry))))
			 (cdr entry))))
	   (false-if-exception (cdr (assoc atom asgs)))))))


; ----------------------
; derived manipulations

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

; ----- ornamental considerations -------
(define-public (ornaments-allow-simultaneous-grab . args)
  (if (not (null? args))
      (let ((id (ornament-get-group-id (car args))))
	(for-each (lambda(orn)
		    (ornament-set-group-id orn id))
		  args))))

(define-public (spectrum-extract-2d-ppm spec x1 y1 xn yn)
  (let* ((s1 (spectrum-extract-ppm spec x1 xn))
	 (s2 (spectrum-transpose s1 1))
	 (s3 (spectrum-extract-ppm s2 y1 yn)))
    (spectrum-transpose s3 1)))




; ------- assignment lists ------

; example assignment:
; (1 (assignments (CG . 31.5) (CB . 32.86) (CA . 60.8575508211657) (N . 122.058562392438) (H . 8.22739647389697)) (residue-type . MET) (verified . #f))

(define-public (assignment-get-ppm asg . atoms)
  (if (null? atoms)
      #f
      (or (assoc-ref (assoc-ref asg 'assignments) (car atoms))
	  (apply assignment-get-ppm asg (cdr atoms)))))
(define-public (assignment-verified? asg)
  (assoc-ref asg 'verified))

;; usage: assign-dist asg-1 asg-2 atom-name-1 weight-1 atom-name-2 weight-2 ...
(define-public (assignment-distance a1 a2 . atoms)
  (define (square x)
    (expt x 2.0))
  (define (dist name weight)
    (square (/ (- (assignment-get-ppm a1 name)
		  (assignment-get-ppm a2 name)) weight)))
  (define (dist-all remaining)
    (if (null? remaining)
	0
	(+ (dist (car remaining)(cadr remaining))
	   (dist-all (cddr remaining)))))
  (sqrt (dist-all atoms)))

; accept two styles of assignment list
(define-public (assignment-get-from-list assignments residue atom)
  (let ((entry (assoc residue assignments)))
    (and entry
	 (let ((asgs (or (false-if-exception (cdr (assoc 'assignments (cdr entry))))
			 (cdr entry))))
	   (false-if-exception (cdr (assoc atom asgs)))))))

(define-public (assignment-get asg atom)
  (let ((asgs (or (false-if-exception (cdr (assoc 'assignments (cdr asg))))
		  (cdr asg))))
    (false-if-exception (cdr (assoc atom asgs)))))




