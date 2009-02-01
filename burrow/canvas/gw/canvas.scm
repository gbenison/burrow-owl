
(define-module (burrow canvas)
  #:duplicates (last))

(use-modules (oop goops)
	     (gnome gtk)
	     (burrow spectrum)
	     (burrow canvas-gw))

;; re-export bindings from g-wrap
(module-use! (module-public-interface (current-module))
	     (resolve-interface '(burrow canvas-gw)))

; ------------- canvas commands -------------

(define (canvas-ensure-contour-plot canv)
  (let loop ((n 0))
    (let ((candidate (canvas-get-item canv n)))
      (cond ((not candidate)(canvas-add-item canv (make <hos-contour-plot>)))
	    ((is-a? candidate <hos-contour-plot>) candidate)
	    (else (loop (+ n 1)))))))

(define-public (canvas-set-spectrum canv spec)
  (let ((contour-plot (canvas-ensure-contour-plot canv)))
    (canvas-sync-world canv spec)
    (set contour-plot 'spectrum spec)))

(define-public (canvas-sync-world canv spec)
  (canvas-set-world canv
		    (spectrum-orig-ppm spec 0)
		    (spectrum-giro-ppm spec 1)
		    (spectrum-giro-ppm spec 0)
		    (spectrum-orig-ppm spec 1)))

(define-public (canvas-set-thres canv thres)
  (warn "use of deprecated function canvas-set-thres")
  (let* ((contour-plot (canvas-ensure-contour-plot canv))
	 (contour (get contour-plot 'contour)))
    (contour-set-thres-adjustment contour thres)))

(define-public (canvas-set-draw-negative canv neg?)
  (warn "use of deprecated function canvas-set-draw-negative")
  (let* ((contour-plot (canvas-ensure-contour-plot canv))
	 (contour (get contour-plot 'contour)))
    (contour-set-draw-negative contour neg?)))

(define-public (contour-plot-set-draw-negative contour-plot neg?)
  (contour-set-draw-negative (get contour-plot 'contour) neg?))

; ----- ornamental considerations -------
(define-public (ornaments-allow-simultaneous-grab . ornaments)
  (define (process-ornament ornament)
    (connect ornament 'acquire
	     (lambda args
	       (for-each (lambda (other)
			   (if (not (eq? other ornament))
			       (if (get other 'mouse-over)
				   (ornament-acquire other))))
			 ornaments))))
  (for-each process-ornament ornaments))

(define-public (marker-set-movable marker movable)
  (warn "use of deprecated function marker-set-movable; use 'sensitive' property")
  (set marker 'sensitive movable))

(define-public (cursor-set-movable cursor movable)
  (warn "use of deprecated function cursor-set-movable; use 'sensitive' property")
  (set cursor 'sensitive movable))

(define-public (adjustment-for-spectrum spectrum dim)
  (let ((lower (spectrum-giro-ppm spectrum dim))
	(upper (spectrum-orig-ppm spectrum dim)))
    (make <gtk-adjustment>
      #:lower lower
      #:upper upper
      #:value (* 0.5 (+ upper lower))
      #:step-increment (/ (spectrum-sw-ppm spectrum dim) 2000.0))))

(define-public (contour-set-thres-adjustment contour adj)
  (define (sync! . args)
    (set contour 'thres (get adj 'value))
    #f)
  (sync!)
  (connect adj 'value-changed sync!))

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




