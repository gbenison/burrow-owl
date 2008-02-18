
(define-module (burrow assignments)
  #:export (assignment?
	    make-assignment
	    assignment-set!
	    assignment-get
	    assignment-getter
	    assignment:atom-name
	    assignment:residue-name
	    assignment:shift
	    assignment:residue-type
	    assignment:verified?

	    register-assignment-format!
	    assignments-from-file

	    assignment-alist->hash
	    residue:name
	    residue:shift
	    residue:set-shift!
	    residue->alist
	    residues->alist))

; ------ the 'assignment' abstract type ------

(define (assignment? obj)
  (and (list? obj)
       (equal? (car obj) 'assignment)))

(define (assert-assignment msg obj)
  (if (not (assignment? obj))
      (error (string-append msg ": wrong type argument"))))

(define (make-assignment)
  (list 'assignment))

(define (assignment-set! assignment name value)
  (assert-assignment "assignment-set" assignment)
  (set-cdr! assignment (assoc-set! (cdr assignment) name value)))

(define (assignment-get assignment name)
  (assert-assignment "assignment-get" assignment)
  (assoc-ref (cdr assignment) name))

(define (assignment-getter name)
  (lambda (assignment)
    (assignment-get assignment name)))

(define assignment:atom-name    (assignment-getter 'atom-name))
(define assignment:residue-name (assignment-getter 'residue-name))
(define assignment:shift        (assignment-getter 'shift))
(define assignment:residue-type (assignment-getter 'residue-type))
(define assignment:verified?    (assignment-getter 'verified))

; ------ Reading/writing assignments --------

(define assignment-formats '())
(define (make-assignment-format name writer reader)
  (list name writer reader))
(define assignment-format:name   car)
(define assignment-format:reader cadr)
(define assignment-format:writer caddr)
(define (register-assignment-format! name reader writer)
  (set! assignment-formats
	(cons (make-assignment-format name reader writer)
	      assignment-formats)))

(define (assignments-from-file fname)
  (define (try-formats formats)
    (define (try-first-format)
      (define reader (assignment-format:reader (car formats)))
      (reader fname))
    (define (try-remaining-formats . args)
      (try-formats (cdr formats)))
    (if (null? formats)
	(throw 'invalid-file-format)
	(catch 'parse-error try-first-format try-remaining-formats)))
  (try-formats assignment-formats))

(define (with-parse-trap proc)
  (catch #t proc (lambda args (throw 'parse-error))))

; ----- "old-style" .scm assignment format -------

; ----- "new-style" .scm assignment format -------

; the 'new style' .scm assignments file, example:
; (1 (assignments (H . 8.22) (N . 122.05) (CA . 60.85)) (residue-type . MET) (verified . #f))

(define (flatten-list list)
  (apply append list))

(define (new-style:read fname)
  (define (entry->assignments entry)
    (let ((chemical-shifts (assoc-ref (cdr entry) 'assignments))
	  (residue-type    (assoc-ref (cdr entry) 'residue-type))
	  (verified        (assoc-ref (cdr entry) 'verified))
	  (residue-name    (car entry)))
      (map (lambda (chemical-shift)
	     (let ((assignment (make-assignment)))
	       (assignment-set! assignment 'residue-name residue-name)
	       (assignment-set! assignment 'residue-type residue-type)
	       (assignment-set! assignment 'verified     verified)
	       (assignment-set! assignment 'atom-name    (car chemical-shift))
	       (assignment-set! assignment 'shift        (cdr chemical-shift))
	       assignment))
	   chemical-shifts)))
  (with-parse-trap
   (lambda ()(flatten-list (map entry->assignments (file->list fname))))))

(define (new-style:write assignments)
  (throw 'not-implemented))

(register-assignment-format! 'scm-style-2 new-style:read new-style:write)

; ----- BMRB assignment format -------

; ------ 'residue' concepts ------
	 
; convert my-alist into a doubly-dereferenced hash
; indexed on residue ID and then atom type
(define (assignment-alist->hash my-alist)
  (let ((residues (make-hash-table 31)))
    (define (ensure-hash resid)
      (let ((result (hash-ref residues resid)))
	(if result
	    result
	    (hash-set! residues resid (make-hash-table 5)))))
    (for-each
     (lambda (asg)
       (let ((this-residue (ensure-hash (assoc-ref asg 'Atom_chem_shift.Seq_ID))))
	 (hash-set! this-residue
		    (assoc-ref asg 'Atom_chem_shift.Atom_ID)
		    asg)))
     my-alist)
    residues))

;
; A 'residue' is defined as
; a residue name and
; a dictionary mapping atom names
; to assignments.
;

(define (residue->alist residue)
  (map cdr (hash-fold acons '()(cdr residue))))

(define (residues->alist residues)
  (apply append
	 (map residue->alist residues)))

(define (residue:shift res name)
  (assoc-ref (hash-ref (cdr res) name) 'Atom_chem_shift.Val))

(define (residue:set-shift! res name value)
  (let ((assignment
	 (assoc 'Atom_chem_shift.Val (hash-ref (cdr res) name))))
    (set-cdr! assignment value)))

(define residue:name car)


; ---- utilities ----

;; FIXME here is how to load 'starparse' failing gracefully if
;; it is not present
(define (module-use%safe path)
  (define module #f)
  (false-if-exception
   (set! module (resolve-interface path)))
  (if module
      (module-use! (current-module) module))
  module)

(define (file->list fname)
  (let ((file (open-input-file fname)))
    (define (read-item)
      (let ((item (read file)))
	(if (eof-object? item)
	    '()
	    (cons item (read-item)))))
    (read-item)))

(define (assoc-ref alist key)
  (false-if-exception
   (cdr (assoc key alist))))

(define (key->getter key)
  (lambda (asg)
    (assoc-ref asg key)))
