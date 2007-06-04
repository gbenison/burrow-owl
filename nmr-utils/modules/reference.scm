#!/bin/sh
#-*-scheme-*-
exec guile -s $0 "$@"
!#

;; GCB 27jan06
;;
;; grok the bruker acqu files
;; use extracted values to get referencing informations

(define-module (nmr reference))

; (use-modules 
             ; (ice-9 rdelim)
	     ; (ice-9 format)
	     ; (srfi srfi-1))


;; from BMRB: relative frequencies for 0ppm standard nucleus
(define get-relative-frequency
  (let ((freq-table
	 '((H 1.000)
	   (h 1.000)
	   (H1 1.000)
	   (N15 0.101329118)
	   (N 0.101329118)
	   (n 0.101329118)
	   (C 0.251449530)
	   (c 0.251449530)
	   (C13 0.251449530))))
    (lambda (nuc)
      (cond ((assoc nuc freq-table) => cadr)
	    (else #f)))))
	
(define (get-frequency-ratio nuc-1 nuc-2)
  (/ (get-relative-frequency nuc-1)
     (get-relative-frequency nuc-2)))

;; return the carrier in ppm for nuc-2 based on
;; the carrier ppm-1 for nuc-1, using exact
;; spectrometer frequencies (in MHz) sf-1 and sf-2
(define-public (reference
	 nuc-1 sf-1 ppm-1
	 nuc-2 sf-2)
  (let* ((hz-1 (* ppm-1 sf-1))
	 (sf0-1 (- (* 1e6 sf-1) hz-1))
	 (sf0-2 (* sf0-1 (get-frequency-ratio nuc-2 nuc-1)))
	 (hz-2 (* sf-2 1e6))
	 (sigma-2 (/ (- hz-2 sf0-2) sf0-2))
	 (ppm-2 (* sigma-2 1e6)))
    ppm-2))









