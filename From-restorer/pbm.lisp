;;; Not completely general.  It does not handle comments. 
(defun read-pbm (filename)
  (with-open-file (stream filename
			  :direction :input
			  :element-type '(unsigned-byte 8))
    (let ((width 0)
	  (height 0))
      (unless (eql (read-byte stream) #.(char-code #\P))
	(error "not a pbm file"))
      (unless (eql (read-byte stream) #.(char-code #\4))
	(error "not a pbm file"))
      ;; read the newline
      (read-byte stream)
      (loop for byte = (read-byte stream)
	    for weight = (digit-char-p (code-char byte))
	    until (null weight)
	    do (setf width (+ (* 10 width) weight)))
      (loop for byte = (read-byte stream)
	    for weight = (digit-char-p (code-char byte))
	    until (null weight)
	    do (setf height (+ (* 10 height) weight)))
      (let ((bitmap (make-array (list height width)
				:element-type 'bit
				:initial-element 0))
	    (bytes-per-row (ceiling width 8)))
	(loop for r from 0 below height
	      do (loop for c from 0 below bytes-per-row
		       for byte = (read-byte stream)
		       do (loop for offset downfrom 7 to 0
				for count from 0
				do (when (and (< (+ (* 8 c) count) width)
					      (plusp (logand byte (ash 1 offset))))
				     (setf (bit bitmap r (+ (* 8 c) count)) 1)))))
	bitmap))))
	
(defun row-slice (bitmap row)
  (let ((width (array-dimension bitmap 1))
	(col 0)
	(slice '()))
    (loop while (< col width)
	  do (loop while (and (< col width)
			      (= 0 (bit bitmap row col)))
		   do (incf col))
	     (when (< col width)
	       (let ((start col))
		 (loop while (and (< col width)
				  (= 1 (bit bitmap row col)))
		       do (incf col))
		 (push (cons start col) slice))))
    (nreverse slice)))

(defun intervals-overlap-p (i1 i2)
  (and (< (car i1) (cdr i2))
       (< (car i2) (cdr i1))))

(defun intervals-overlapping-intervals (intervals1 intervals2)
  (loop for i in intervals2
	when (find-if (lambda (ii) (intervals-overlap-p i ii)) intervals1)
	  collect i))

;;; A slice is a list of non-overlapping intervals sorted by
;;; increasing coordinates.

;;; Return true if and only if at least one interval in the first slice
;;; overlaps with at least one interval in the second slice.
(defun slices-overlap-p (slice1 slice2)
  (loop until (or (null slice1) (null slice2))
	for i1 = (car slice1)
	for i2 = (car slice2)
	do (cond ((< (cdr i1) (car i2))
		  ;; The first interval ends before the second
		  ;; one starts.  Remove the first interval.
		  (pop slice1))
		 ((< (cdr i2) (car i1))
		  ;; The second interval ends befor the first
		  ;; one starts.  Remove the second interval.
		  (pop slice2))
		 (t
		  ;; The first two intervals overlap.
		  ;; Declare success.
		  (return t)))))

(defun interval-extends-blob-p (blob interval)
  (slices-overlap-p (car blob) (list interval)))

(defun blobs-extended-by-interval (blobs interval)
  (remove-if-not (lambda (blob) (interval-extends-blob-p blob interval))
		 blobs))

;;; Take a list of slices and compute a single slice containing each
;;; interval of each slice.  No interval in one slice overlaps with
;;; any interval in any other slice.
(defun merge-slices (slices)
  (sort (reduce #'append slices :from-end t) #'< :key #'car))

;;; Merge a list of blobs into a single blob.
;;; Here an blob is a list of slices.  The first slice of every
;;; blob has the same vertical coordinate.  
(defun merge-blobs (blobs)
  (loop until (every #'null blobs)
	collect (prog1 (merge-slices (mapcar #'car blobs))
		  (setf blobs (mapcar #'cdr blobs)))))

;;; Divide a list into two sublists according to a predicate.  The
;;; relative order of the elements is preserved in both resulting
;;; lists.
(defun divide-list (list predicate)
  (let ((true '())
	(false '()))
    (loop for element in list
	  do (if (funcall predicate element)
		 (push element true)
		 (push element false)))
    (values (nreverse true) (nreverse false))))
	
;;; Divide a list of blobs into two parts, the blobs that do not
;;; overlap any interval in the slice, and the blobs that do.
(defun split-blobs (blobs intervals)
  (let ((non-overlapping '())
	(overlapping '()))
    (loop for blob in blobs
	  do (if (slices-overlap-p (car blob) intervals)
		 (push blob overlapping)
		 (push blob non-overlapping)))
    (values non-overlapping overlapping)))
		  

(defun merge-touching-blobs (blobs slice)
  (let ((merged-blobs blobs))
    (loop for interval in slice
	  do (multiple-value-bind (to-merge remaining)
		 (split-list merged-blobs
			     (lambda (blob)
			       (interval-extends-blob-p blob interval)))
	       (cond ((null to-merge)
		      nil)
		     ((null (cdr to-merge))
		      (setf merged-blobs
			    (append to-merge remaining)))
		     (t
		      (setf merged-blobs
			    (cons (merge-blobs to-merge) remaining))))))
    merged-blobs))
    
(defun extend-blob (blob slice)
  (multiple-value-bind (extension remaining)
      (split-list slice
		  (lambda (interval) (interval-extends-blob-p blob interval)))
    (values (cons extension blob) remaining)))

(defun extend-merged-blobs (blobs slice)
  (let ((remaining-slice slice)
	(extended-blobs '()))
    (loop for blob in blobs
	  do (multiple-value-bind (extended-blob remaining)
		 (extend-blob blob remaining-slice)
	       (push extended-blob extended-blobs)
	       (setf remaining-slice remaining)))
    (values (nreverse extended-blobs) remaining-slice)))

(defun extend-blobs (blobs slice)
  (extend-merged-blobs (merge-touching-blobs blobs slice) slice))
  
(defun process-unfinished-blobs (blobs slice)
  (multiple-value-bind (extended-blobs remaining-slice)
      (extend-blobs blobs slice)
    (append extended-blobs
	    (mapcar (lambda (slice) (list (list slice)))
		    remaining-slice))))

(defun process-row (blobs slice)
  (multiple-value-bind (unfinished finished)
      (split-list blobs
		  (lambda (blob)
		    (find-if (lambda (interval)
			       (interval-extends-blob-p blob interval))
			     slice)))
    (values finished
	    (process-unfinished-blobs unfinished slice))))

(defun normalize-blob (blob row)
  (let ((l (reduce #'min blob :key (lambda (slice) (car (car slice))))))
    (list* row l (loop for slice in blob
		       collect (loop for interval in slice
				     collect (cons (- (car interval) l)
						   (- (cdr interval) l)))))))

(defun find-blobs (bitmap start-row end-row)
  (let ((finished-blobs '())
	(active-blobs '()))
    (loop for row downfrom (1- end-row) to start-row
	  do (let ((slice (row-slice bitmap row)))
	       (multiple-value-bind (finished active)
		   (process-row active-blobs slice)
		 (setf finished-blobs
		       (append finished-blobs
			       (mapcar (lambda (blob)
					 (normalize-blob blob row))
				       finished)))
		 (setf active-blobs active))))
    (append finished-blobs
	    (mapcar (lambda (blob)
		      (normalize-blob blob start-row))
		    active-blobs))))
		 
(defun area (blob)
  (loop for slice in (cddr blob)
	sum (loop for interval in slice
		  sum (- (cdr interval) (car interval)))))

(defun height (blob)
  (length (cddr blob)))

(defun width (blob)
  (- (reduce #'max (cddr blob) :key (lambda (slice) (cdr (car (last slice)))))
     (reduce #'min (cddr blob) :key (lambda (slice) (car (car slice))))))

(defun histogram (values)
  (let ((histogram (make-array (1+ (reduce #'max values)) :initial-element 0)))
    (loop for value in values
	  do (incf (aref histogram value)))
    (terpri)
    (loop for r from 0 below (array-dimension histogram 0)
	  do (format t "~2d: " r)
	     (loop repeat (floor (aref histogram r) 4)
		   do (format t "*"))
	     (terpri))))

(defun tree-size (tree)
  (let ((table (make-hash-table)))
    (labels ((aux (tree)
	       (if (or (atom tree) (gethash tree table))
		   0
		   (progn (setf (gethash tree table) t)
			  (+ 1 (aux (car tree)) (aux (cdr tree)))))))
      (aux tree))))

(defun compact-tree (tree)
  (let ((table (make-hash-table :test #'equal)))
    (labels ((aux (tree)
	       (cond ((atom tree) tree)
		     ((gethash tree table))
		     (t
		      (let ((new (cons (aux (car tree)) (aux (cdr tree)))))
			(setf (gethash new table) new)
			new)))))
      (aux tree))))

(defclass line ()
  ((%blobs :initform '() :initarg :blobs :accessor blobs)
   (%baseline :initform '() :accessor baseline)))

(defun horizontally-aligned-p (blob1 blob2)
  (< (abs (- (car blob1) (car blob2)))
     (* (abs (- (cadr blob1) (cadr blob2))) 0.1)))

(defun make-lines (blobs)
  (let ((sorted-blobs (sort (copy-list blobs) #'< :key #'cadr))
	(lines '()))
    (loop for blob in sorted-blobs
	  do (let ((existing-line (find blob lines
					:key (lambda (line) (car (blobs line)))
					:test #'horizontally-aligned-p)))
	       (if (null existing-line)
		   (push (make-instance 'line :blobs (list blob)) lines)
		   (push blob (blobs existing-line)))))
    (loop for line in lines
	  do (setf (blobs line) (nreverse (blobs line))))
    (sort lines #'<
	  :key (lambda (line) (car (car (blobs line)))))))
    
(defun start (interval)
  (car interval))

(defun end (interval)
  (cdr interval))

(defun interval-length (interval)
  (- (end interval) (start interval)))

(defun slice-length (slice)
  (reduce #'+ slice :key #'interval-length))

(defun slice-pixel-difference (slice1 slice2)
  (cond ((and (null slice1) (null slice2))
	 0)
	((null slice1)
	 (slice-length slice2))
	((null slice2)
	 (slice-length slice1))
	((<= (end (car slice1)) (start (car slice2)))
	 (+ (interval-length (car slice1))
	    (slice-pixel-difference (cdr slice1) slice2)))
	((<= (end (car slice2)) (start (car slice1)))
	 (+ (interval-length (car slice2))
	    (slice-pixel-difference slice1 (cdr slice2))))
	((< (start (car slice1)) (start (car slice2)))
	 (+ (- (start (car slice2)) (start (car slice1)))
	    (slice-pixel-difference
	     (cons (cons (start (car slice2)) (end (car slice1))) (cdr slice1))
	     slice2)))
	((< (start (car slice2)) (start (car slice1)))
	 (+ (- (start (car slice1)) (start (car slice2)))
	    (slice-pixel-difference
	     slice1
	     (cons (cons (start (car slice1)) (end (car slice2))) (cdr slice2)))))
	((< (end (car slice1)) (end (car slice2)))
	 (slice-pixel-difference
	  (cdr slice1)
	  (cons (cons (end (car slice1)) (end (car slice2))) (cdr slice2))))
	((< (end (car slice2)) (end (car slice1)))
	 (slice-pixel-difference
	  (cons (cons (end (car slice2)) (end (car slice1))) (cdr slice1))
	  (cdr slice2)))
	(t
	 (slice-pixel-difference (cdr slice1) (cdr slice2)))))

(defun blob-pixel-difference (blob1 blob2)
  (let ((slices1 (cddr blob1))
	(slices2 (cddr blob2))
	(difference 0))
    (loop until (or (null slices1) (null slices2))
	  do (incf difference
		   (slice-pixel-difference (car slices1) (car slices2)))
	     (pop slices1)
	     (pop slices2))
    (loop for slice in slices1
	  do (incf difference (slice-length slice)))
    (loop for slice in slices2
	  do (incf difference (slice-length slice)))
    difference))
    