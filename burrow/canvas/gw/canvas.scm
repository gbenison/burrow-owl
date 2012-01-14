
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
    (set contour-plot 'spectrum spec)
    contour-plot))

(define-public (canvas-sync-world canv spec)
  (canvas-set-world canv
		    (spectrum-orig-ppm spec 0)
		    (spectrum-giro-ppm spec 1)
		    (spectrum-giro-ppm spec 0)
		    (spectrum-orig-ppm spec 1)))

;;
;; Synchronize focus property of @canvas with
;; <gtk-adjustment>s @x-adjustment @y-adjustment
;;
(define-public (canvas-tie-focus canvas x-adjustment y-adjustment)
  (define (canvas:sync . args)
    (canvas-set-focus canvas
		      (get x-adjustment 'value)
		      (get y-adjustment 'value)))
  (canvas:sync)
  (connect x-adjustment 'value-changed canvas:sync)
  (connect y-adjustment 'value-changed canvas:sync)
  (connect canvas 'scroll-focus
	   (lambda (c x y)
	     (set x-adjustment 'value x)
	     (set y-adjustment 'value y))))

;;
;; Synchronize focus property of @canvas with
;; position of @marker
;;
(define-public (canvas-tie-focus-to-marker canvas marker)
  (canvas-set-focus canvas
		    (get marker 'x)
		    (get marker 'y))
  (connect marker
	   'dropped
	   (lambda (m x y)
	     (canvas-set-focus canvas x y)))
  (connect canvas
	   'scroll-focus
	   (lambda (c x y)
	     (set marker 'x x)
	     (set marker 'y y))))

(define-public (canvas-set-thres canv thres)
  (warn "use of deprecated function canvas-set-thres")
  (let* ((contour-plot (canvas-ensure-contour-plot canv))
	 (contour (get contour-plot 'contour)))
    (contour-set-thres-adjustment contour thres)))

;;
;; Accept the current threshold level of @contour-plot as the default
;; level; create a new GtkAdjustment spanning the default value and tie
;; it to the contour threshold.
;;
(define-public (contour-plot->thres-adjustment contour-plot)
  (let* ((contour (get contour-plot 'contour))
	 (default-threshold (get contour 'threshold))
	 (thres-adjustment
	  (make <gtk-adjustment>
	    #:lower (- default-threshold 2)
	    #:upper (+ default-threshold 5)
	    #:value default-threshold
	    #:step-increment 0.1)))
    (contour-set-thres-adjustment contour thres-adjustment)
    thres-adjustment))

(define-public (canvas-set-draw-negative canv neg?)
  (warn "use of deprecated function canvas-set-draw-negative")
  (let* ((contour-plot (canvas-ensure-contour-plot canv))
	 (contour (get contour-plot 'contour)))
    (contour-set-draw-negative contour neg?)))

(define-public (contour-plot-set-draw-negative contour-plot neg?)
  (contour-set-draw-negative (get contour-plot 'contour) neg?))

; ---- Color schemes for contours ----
(define-public (set-to-purple contour)
  (set contour 'color-positive-low  #(#x0800 #x0 #x6000))
  (set contour 'color-positive-high #(#xf000 #x0 #x8000)))

(define-public (set-to-greenish contour)
  (set contour 'color-positive-low  #(#x0 #x800  #x6000))
  (set contour 'color-positive-high #(#x0 #xf000 #xd000)))

(define-public (set-to-reddish contour)
  (set contour 'color-positive-low  #(#x4000 #x800  #x0))
  (set contour 'color-positive-high #(#xf000 #x6000 #x0)))

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

(define-public (canvas-add-cursor-horizontal canv)
  (warn "use of deprecated function canvas-add-cursor-horizontal; use 'orientation' property")
  (canvas-add-cursor canv 'horizontal))

(define-public (canvas-add-cursor-vertical canv)
  (warn "use of deprecated function canvas-add-cursor-vertical; use 'orientation' property")
  (canvas-add-cursor canv 'vertical))

(define-public (marker-text-set-color marker red green blue)
  (warn "use of deprecated function marker-text-set-color; use 'color' property")
  (set marker 'color (vector red green blue)))

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
    (set contour 'threshold (get adj 'value))
    #f)
  (sync!)
  (connect adj 'value-changed sync!))




