
(define-module (burrow assignments)
  #:export (file->list
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
  (let ((tmp (assoc key alist)))
    (and tmp (cdr tmp))))

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

