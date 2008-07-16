
(define-module (burrow spectrum))

(use-modules (oop goops)
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

(define-public (spectrum-extract-2d-ppm spec x1 y1 xn yn)
  (let* ((s1 (spectrum-extract-ppm spec x1 xn))
	 (s2 (spectrum-transpose s1 1))
	 (s3 (spectrum-extract-ppm s2 y1 yn)))
    (spectrum-transpose s3 1)))

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
  (get connect-after unblock connected? create-signal get-properties set emit block disconnect invoke get-signals get-property-names find-property contour-get-nlvl  painter-redraw dimension-sim-new hos-marker-text-get-type spectrum-project-pt spectrum-stddev   painter-set-xform  spectrum-project-ppm spectrum-peek   painter-view-ppm    spectrum-np spectrum-transpose spectrum-traverse   painter-view-world  spectrum-ndim spectrum-extract spectrum-traverse-blocking contour-set-color-positive hos-painter-get-type  backing-sim-new  spectrum-sw spectrum-extract-ppm hos-spectrum-get-type contour-set-color-negative backing-sim-append-dimension <sim-peak*>  spectrum-sw-ppm spectrum-diagonal-project spectrum-nih-from-file    backing-sim-generate-spectrum  spectrum-sf spectrum-convolute spectrum-nih-2d-from-file  cursor-set-pos  backing-sim-set-pos <sim-profile*> marker-get-pos spectrum-orig spectrum-integrate spectrum-nih-unfold   painter-bwps-new-file  backing-sim-append-peak  spectrum-giro spectrum-cache contour-get-n-contours contour-get-type <gdouble*>  hos-painter-bwps-get-type  sim-peak-product-new  spectrum-orig-ppm spectrum-get-ranked contour-get-levels painter-set-spectrum cursor-get-position  ornament-get-group-id sim-profile-gaussian-new  spectrum-giro-ppm spectrum-get-max contour-set-draw-negative painter-set-contour   sim-peak-product-append-profile  spectrum-ppm2pt spectrum-get-min contour-get-thres  painter-get-contour  hos-backing-sim-get-type  spectrum-pt2ppm spectrum-get-percentile contour-get-factor  painter-cancel-redraws  hos-dimension-sim-get-type  spectrum-project spectrum-mean painter-set-thres sleep-if-requested painter-set-nlvl   assignment-get usage-message bruker-transpose   file->list spectrum-extract-2d-ppm define-usage assignment-get-from-list spectrum-plane-multiply confess assignment-verified?  write-line command-line-argument  painter-set-factor assignment-get-ppm  assignment-distance standard-syms))




