
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

	    assignments->residue-table
	    residue:name
	    residue:shift
	    residue:set-shift!
	    residue->alist
	    residues->alist))

; ---- utilities ----

; similar to (use-modules), but if fails to find module,
; returns false rather than throw an error
(define (use-module%safe path)
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

(define (flatten-list list)
  (apply append list))

(define (type-check predicate obj)
  (if (not (predicate obj))
      (throw 'type-error)))

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

(define (old-style:read fname)
  (define (entry->assignments entry)
    (let* ((chemical-shifts:all (cdr entry))
	   (residue-name        (car entry))
	   (chemical-shifts (filter (lambda (x)(number? (cdr x))) chemical-shifts:all)))
      (map (lambda (chemical-shift)
	     (let ((assignment (make-assignment)))
	       (type-check symbol? (car chemical-shift))
	       (type-check number? (cdr chemical-shift))
	       (assignment-set! assignment 'residue-name residue-name)
	       (assignment-set! assignment 'atom-name    (car chemical-shift))
	       (assignment-set! assignment 'shift        (cdr chemical-shift))
	       assignment))
	   chemical-shifts)))
  (with-parse-trap
   (lambda ()(flatten-list (map entry->assignments (file->list fname))))))

(define (old-style:write assignments)
  (throw 'not-implemented))

(register-assignment-format! 'scm-style-1 old-style:read old-style:write)

; ----- "new-style" .scm assignment format -------

; the 'new style' .scm assignments file, example:
; (1 (assignments (H . 8.22) (N . 122.05) (CA . 60.85)) (residue-type . MET) (verified . #f))

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

(define (value-tidy value)
  (if (number? value)
      (if (integer? value)
	  (inexact->exact value)
	  value)
      (string->symbol value)))

;; map NMR-Star v.2 names to standard NMR-Star v.3 names
(define (ensure-star-v3 name)
  (case name
    ((Residue_seq_code Atom_chem_shift.Auth_seq_ID)
     'Atom_chem_shift.Seq_ID)
    ((Atom_name Atom_chem_shift.Atom_ID Atom_chem_shift.Auth_atom_ID)
     'Atom_chem_shift.Atom_ID)
    ((Chem_shift_value Atom_chem_shift.Val)
     'Atom_chem_shift.Val)
    (else name)))

;; map various NMR-Star names to standard NMR-Star v.2.1 names
(define (ensure-star-v2.1 name)
  (case name
    ((Residue_seq_code Atom_chem_shift.Auth_seq_ID Atom_chem_shift.Seq_ID)
     'Residue_seq_code)
    ((Atom_name Atom_chem_shift.Atom_ID Atom_chem_shift.Auth_atom_ID)
     'Atom_name)
    ((Chem_shift_value Atom_chem_shift.Val)
     'Chem_shift_value)
    ((Residue_label)
     'Residue_label)
    (else name)))

(define (bmrb:read fname)
  (let ((result '())
	(group  '()))
    (define (in-group? name)
      (assoc name group))
    (define (group->assignment group)
      (define (get-component name)
	(cdr (assoc name group)))
      (false-if-exception
       (let ((assignment (make-assignment)))
	 (assignment-set! assignment 'residue-name  (get-component 'Residue_seq_code))
	 (assignment-set! assignment 'atom-name     (get-component 'Atom_name))
	 (assignment-set! assignment 'shift         (get-component 'Chem_shift_value))
	 (assignment-set! assignment 'residue-type  (get-component 'Residue_label))
	 assignment)))
    (define (append-assignment!)
      (let ((assignment (group->assignment group)))
	(if assignment
	    (set! result (cons assignment result))))
      (set! group '()))
    (define (process-entry name value)
      (let ((name  (ensure-star-v2.1 name))
	    (value (value-tidy value)))
	(if (in-group? name)
	    (append-assignment!)
	    (set! group (cons (cons name value) group)))))
    (star-parse fname #f process-entry)
    result))

(define (bmrb:write assignments)
  (throw 'not-implemented))

(if (use-module%safe '(starparse))
    (register-assignment-format! 'bmrb bmrb:read bmrb:write))

; ------ 'residue' concepts ------
(define (assignments->residue-table assignments)
  (let ((residues (make-hash-table 31)))
    (define (ensure-hash resid)
      (let ((result (hash-ref residues resid)))
	(if result result (hash-set! residues resid (list)))))
    (for-each
     (lambda (asg)
       (let ((entry (or (hash-ref residues (assignment:residue-name asg)) '())))
	 (hash-set! residues (assignment:residue-name asg) (cons asg entry))))
     assignments)
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


