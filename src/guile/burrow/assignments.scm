
(define-module (burrow assignments)
  #:export (file->list
	    residue:name
	    residue:shift
	    residue:set-shift!
	    residue->alist
	    residues->alist
	    assignment-alist->hash
	    assignment:name
	    assignment:shift
	    assignment:residue-type
	    assignment:verified?))


; the 'new style' .scm assignments file, with 'verified' tag.
; example assignment:
; (1 (assignments (H . 8.22739647389697) (N . 122.058562392438) (CA . 60.8575508211657) (CB . 32.86) (CG . 31.5)) (residue-type . MET) (verified . #f))

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

(define assignment:name car)
(define (assignment:shift asg atom)
  (let ((assigns (assoc-ref (cdr asg) 'assignments)))
    (if assigns
	(assoc-ref assigns atom)
	(assoc-ref (cdr asg) atom))))
(define (assignment:residue-type asg)
  (assoc-ref (cdr asg) 'residue-type))
(define (assignment:verified? asg)
  (assoc-ref (cdr asg) 'verified))
	 
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
