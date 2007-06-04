#!/bin/sh
#-*-scheme-*-
exec guile -s $0 "$@"
!#

;; GCB 27jan06
;;
;; grok the bruker acqu files
;; use extracted values to get referencing informations

(define-module (nmr acqu-grok))

(use-modules (ice-9 rdelim)
	     (ice-9 format)
	     (srfi srfi-1))

(export acqu->alist
	parse-gather
	parse-single)

; ----- utilities ----

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ 1 a) b))))

; --------------------

; example regexp code:

; an example of a single parameter: 
; ##$BF4= 600.03
(define rx:line-single (make-regexp "^##.([^ ]+)= (.*)$"))

;; an example of an array parameter:
;##$AMP= (0..31)
;100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 
;100 100 100 100 100 100 100 100 100 100 100 100 100 100
; note: special characters e.g. ) must be backslash-escaped twice, once due
; to scheme string parsing and once due to regex parsing.
(define rx:line-array (make-regexp "^##.([^ ]+)= \\([0-9]+\\.\\.[0-9]+\\)$"))

(define (grab-lines infile-name)
  (with-input-from-file infile-name
    (lambda()
	  (let ((rx:delim (make-regexp "^##\\$")))
	    (define (grab-one result line-list)
	      (let ((line (read-line)))
		(if (eof-object? line)
		    result
		    (if (regexp-exec rx:delim line)
			(grab-one (append result (list line-list))
				  (list line))
			(grab-one result (append line-list (list line)))))))
	    (grab-one '() '())))))

(define (lines->list lines)
  (define (string-full? str)
    (> (string-length str) 0))
  (filter string-full?
	  (apply append
		 (map (lambda (line)
			(string-split line #\ ))
		      lines))))

(define (grok-lines lines)
  (map
   (lambda (line)
     (let ((first-line (car line)))
       (let ((match:array (regexp-exec rx:line-array first-line)))
	 (if match:array
	     (let ((name (string->symbol (match-get-elem match:array 1))))
	       (cons name (lines->list (cdr line))))
	     (let ((match:single (regexp-exec rx:line-single first-line)))
	       (and match:single
		    (cons (string->symbol (match-get-elem match:single 1))
			  (match-get-elem match:single 2))))))))
   lines))


; example of a regex match object:
; #(##$ZL4= 120 (0 . 11) (3 . 6) (8 . 11))
(define (match-get-elem match elem)
  (let ((index (+ elem 1)))
    (if (> index (vector-length match))
	#f
	(let ((str (vector-ref match 0))
	      (limits (vector-ref match index)))
	  (substring str (car limits)(cdr limits))))))

(define (acqu->alist fname)
  (grok-lines (grab-lines fname)))

(define (acqu-grok fname)
  (let ((table (grok-lines (grab-lines fname))))
    (table->lookup table)))

(define (table->lookup table)
  (lambda (name . rest)
    (define (get-entry-n name n)
      (let ((entry (assoc name table)))
	(and entry
	     (if (list? (cdr entry))
		 (list-ref (cdr entry) n)
		 (cdr entry)))))
    (if (null? rest)
	(get-entry-n name 1)
	(get-entry-n name (car rest)))))

(define (get-entry-numeric name . rest)
  (let ((result (apply get-entry name rest)))
    (if (string? result)
	(string->number result))))

; ----- start referencing section -----


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

;; sf in MHz;
(define (reference
	 nuc-1 sf-1 ppm-1
	 nuc-2 sf-2)
  (let* ((hz-1 (* ppm-1 sf-1))
	 (sf0-1 (- (* 1e6 sf-1) hz-1))
	 (sf0-2 (* sf0-1 (get-frequency-ratio nuc-2 nuc-1)))
	 (hz-2 (* sf-2 1e6))
	 (sigma-2 (/ (- hz-2 sf0-2) sf0-2))
	 (ppm-2 (* sigma-2 1e6)))
    ppm-2))

(define (verbose-reference
	 nuc-1 sf-1 ppm-1
	 nuc-2 sf-2)
  (let ((hz-1 (* ppm-1 sf-1)))
    (format #t "nucleus 1 offset ~a ppm (= ~a hz)~%" ppm-1 hz-1)
    (let ((sf0-1 (- (* 1e6 sf-1) hz-1)))
      (format #t "zero frequency of nucleus 1 = ~a hz~%" sf0-1)
      (let* ((ratio (get-frequency-ratio nuc-2 nuc-1))
	     (sf0-2 (* sf0-1 ratio)))
	(format #t "zero frequency of nucleus 2 = ~a hz using ratio ~a~%"
		sf0-2 ratio)
	(let* ((hz-2 (* 1e6 sf-2))
	       (delta-hz-2 (- hz-2 sf0-2))
	       (ppm-2 (* 1e6 (/ delta-hz-2 sf0-2))))
	  (format #t "nuc 2 freq. = ~a (~a ppm)~%" delta-hz-2 ppm-2))))))



; an example of a single parameter: 
; ##$BF4= 600.03
(define rx:single (make-regexp "^##.([^ ]+)= ([^(].*)$"))

; an example of an array parameter:
;##$AMP= (0..31)
;100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 
;100 100 100 100 100 100 100 100 100 100 100 100 100 100
; note: special characters e.g. ) must be backslash-escaped twice, once due
; to scheme string parsing and once due to regex parsing.
(define rx:array (make-regexp "^##.([^ ]+)= \\([0-9]+\\.\\.[0-9]+\\)$"))

(define (parse-file fname cb:single cb:array)
  (define (parse-top line file)
    (if (not (eof-object? line))
	(let ((single (regexp-exec rx:single line)))
	  (if single
	      (begin (cb:single (match-get-elem single 1)
				(match-get-elem single 2))
		     (parse-top (read-line file) file))
	      (let ((array (regexp-exec rx:array line)))
		(if array
		    (parse-array-lines (match-get-elem array 1) (list) (read-line file) file)
		    (parse-top (read-line file) file)))))))
  (define (parse-array-lines name result line file)
    (if (or (eof-object? line)
	    (regexp-exec rx:single line)
	    (regexp-exec rx:array line))
	(begin
	  (cb:array name result)
	  (parse-top line file))
	(parse-array-lines name (cons line result) (read-line file) file)))
  (let ((port (open-input-file fname)))
    (parse-top (read-line port) port)))

;; a value-grokkin' callback
(define (parse-gather fname keys)
  (let ((results (list)))
    (parse-file fname
		(lambda (name val)  ; single proc
		  (if (member name keys)
		      (set! results (assoc-set! results (string->symbol name) val))))
		(lambda (name val)  ; array proc
		  (if (member name keys)
		      (set! results (assoc-set! results
						(string->symbol name)
						(string-split (apply string-append val) #\ ))))))
    results))

;; a value-grokkin' callback
(define (parse-single fname key)
  (let ((result #f))
    (parse-file fname
		(lambda (name val)  ; single proc
		  (if (equal? name key)
		      (set! result val)))
		(lambda (name val) #f))
    result))








