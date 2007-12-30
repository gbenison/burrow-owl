;; Some extra utilities
;; useful with convoluted spectra.

(define-module (spec-extra))

(use-modules (oop goops)
	     (gnome gtk)
	     (spectrum))

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

(define-public (adjustment-for-spectrum spectrum dim)
  (make <gtk-adjustment>
    #:lower (spectrum-giro-ppm spectrum dim)
    #:upper (spectrum-orig-ppm spectrum dim)
    #:step-increment (/ (spectrum-sw-ppm spectrum dim) 2000.0)))
	




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


; ------------- deperecated & disabled commands -----------

(define-macro (declare-disabled-symbol symbol)
  `(if (not (defined? (quote ,symbol)))
       (begin
	 (define ,symbol
	   (lambda args
	     (error
	      (format #f "use of disabled symbol \"~a\" -- is this an old script? burrow version is ~a" (quote ,symbol)(burrow-version)))))
	 (export ,symbol))))

(define-macro (declare-disabled-symbols symbols)
  (cons 'begin (map (lambda (symbol)(list 'declare-disabled-symbol symbol)) symbols)))

;; check for disabled symbols from burrow-1.2
(declare-disabled-symbols
  (get connect-after unblock connected? create-signal get-properties set emit block disconnect invoke get-signals get-property-names find-property contour-get-nlvl box-get-y1 painter-redraw canvas-view2ppm ornament-sync-region dimension-sim-new hos-marker-text-get-type spectrum-project-pt spectrum-stddev contour-set-thres-adjustment box-get-yn painter-set-xform canvas-ppm2view ornament-pick-up marker-set-size cursor-set-orientation spectrum-project-ppm spectrum-peek contour-set-factor-adjustment canvas-add-box painter-view-ppm canvas-view2pt adjustment-for-spectrum marker-set-movable cursor-get-adjustment spectrum-np spectrum-transpose spectrum-traverse contour-set-nlvl-adjustment hos-box-get-type painter-view-world canvas-pt2view hos-ornament-get-type marker-set-adjustments cursor-set-movable spectrum-ndim spectrum-extract spectrum-traverse-blocking contour-set-color-positive hos-painter-get-type hos-canvas-get-type backing-sim-new marker-get-x-adjustment cursor-set-enabled spectrum-sw spectrum-extract-ppm hos-spectrum-get-type contour-set-color-negative painter-gdk-new ornament-overlap-region backing-sim-append-dimension <sim-peak*> marker-get-y-adjustment spectrum-sw-ppm spectrum-diagonal-project spectrum-nih-from-file contour-get-thres-adjustment cursor-set-adjustment painter-gdk-set-drawable-gc ornament-redraw backing-sim-generate-spectrum canvas-add-marker spectrum-sf spectrum-convolute spectrum-nih-2d-from-file contour-get-factor-adjustment cursor-set-pos hos-painter-gdk-get-type ornament-release backing-sim-set-pos <sim-profile*> marker-get-pos spectrum-orig spectrum-integrate spectrum-nih-unfold contour-get-nlvl-adjustment canvas-add-cursor-horizontal painter-bwps-new-file ornament-move backing-sim-append-peak marker-set-pos spectrum-giro spectrum-cache contour-get-n-contours contour-get-type <gdouble*> canvas-add-cursor-vertical hos-painter-bwps-get-type ornament-test-grab sim-peak-product-new hos-marker-get-type spectrum-orig-ppm spectrum-get-ranked contour-get-levels painter-set-spectrum cursor-get-position canvas-get-painter ornament-get-group-id sim-profile-gaussian-new marker-text-set-label spectrum-giro-ppm spectrum-get-max contour-set-draw-negative painter-set-contour hos-cursor-get-type canvas-set-painter ornament-set-group-id sim-peak-product-append-profile canvas-add-marker-text spectrum-ppm2pt spectrum-get-min contour-get-thres box-get-x1 painter-get-contour canvas-get-spectrum ornament-set-region hos-backing-sim-get-type marker-text-set-patch spectrum-pt2ppm spectrum-get-percentile contour-get-factor box-get-xn painter-cancel-redraws canvas-add-ornament ornament-invalidate-region hos-dimension-sim-get-type marker-text-set-color spectrum-project spectrum-mean painter-set-thres sleep-if-requested painter-set-nlvl canvas-set-spectrum ornaments-allow-simultaneous-grab assignment-get usage-message bruker-transpose canvas-set-thres set-to-reddish file->list spectrum-extract-2d-ppm define-usage assignment-get-from-list spectrum-plane-multiply confess assignment-verified? set-to-greenish write-line command-line-argument canvas-set-draw-negative painter-set-factor assignment-get-ppm set-to-purple assignment-distance standard-syms))




