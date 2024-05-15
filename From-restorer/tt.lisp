;;; Pick a rectangle of some uneven dimensions.

(defparameter *width* 5.2d0)
(defparameter *height* 7.7d0)

(defparameter *rotation* (exp (complex 0d0 (* pi 0.1d0))))

(defparameter *c1* #c(0d0 0d0))
(defparameter *c2* (* (complex *width* 0d0) *rotation*))
(defparameter *c3* (* (complex *width* *height*) *rotation*))
(defparameter *c4* (* (complex 0d0 *height*) *rotation*))

(defparameter *w* (realpart (- *c2* *c4*)))
(defparameter *h* (imagpart (- *c3* *c1*)))

(incf *c1* (- (realpart *c4*)))
(incf *c2* (- (realpart *c4*)))
(incf *c3* (- (realpart *c4*)))
(incf *c4* (- (realpart *c4*)))

(defun point-to-the-left-of-line-p (p a b)
  (plusp (phase (/ (- p a) (- b a)))))

(defun point-inside-rectangle-p (p c1 c2 c3 c4)
  (and (point-to-the-left-of-line-p p c1 c2)
       (point-to-the-left-of-line-p p c2 c3)
       (point-to-the-left-of-line-p p c3 c4)
       (point-to-the-left-of-line-p p c4 c1)))

(defun create-bitmap (c1 c2 c3 c4)
  (let ((w (+ 2 (ceiling *w*)))
	(h (+ 2 (ceiling *h*))))
    (flet ((pixel-inside-p (r c)
	     (let ((count 0))
	       (loop for rr from r by 0.1d0
		     repeat 10
		     do (loop for cc from c by 0.1d0
			      repeat 10
			      do (when (point-inside-rectangle-p
					(complex cc rr)
					c1 c2 c3 c4)
				   (incf count))))
	       (>= count 50))))
      (let ((bitmap (make-array (list h w)
				:element-type 'bit
				:initial-element 0)))
	(loop for r from 0 below h
	      do (loop for c from 0 below w
		       do (when (pixel-inside-p r c)
			    (setf (bit bitmap r c) 1))))
	bitmap))))

(defun create-n-bitmaps (n c1 c2 c3 c4)
  (loop repeat n
	collect (let ((offset (complex (random 2d0) (random 2d0))))
		  (create-bitmap (+ c1 offset)
				 (+ c2 offset)
				 (+ c3 offset)
				 (+ c4 offset)))))
		      

(defun print-bitmap (bitmap)
  (loop for r from 0 below (array-dimension bitmap 0)
	do (loop for c from 0 below (array-dimension bitmap 1)
		 do (format t "~c" (if (zerop (bit bitmap r c)) #\. #\*)))
	   (format t "~%")))
				
(defun oversample-bitmap (bitmap)
  (let* ((hh (array-dimension bitmap 0))
	 (h (* 4 hh))
	 (ww (array-dimension bitmap 1))
	 (w (* 4 ww))
	 (result (make-array (list h w) :element-type 'bit)))
    ;; Fill in default before filtering.
    (loop for r from 0 below h
	  do (loop for c from 0 below w
		   do (setf (bit result r c)
			    (bit bitmap (floor r 4) (floor c 4)))))
    (flet ((b (r c)
	     (if (and (>= r 0) (< r hh) (>= c 0) (< c ww))
		 (bit bitmap r c)
		 0)))
      (flet ((do-upper-left (r c)
	       (cond ((and (= 0 (b (- r 1) c))
			   (= 0 (b r (- c 1)))
			   (= 0 (b (- r 1) (- c 1))))
		      (setf (bit result (* 4 r) (* 4 c)) 0))
		     ((and (= 1 (b (- r 1) c))
			   (= 1 (b r (- c 1)))
			   (= 1 (b (- r 1) (- c 1))))
		      (setf (bit result (* 4 r) (* 4 c)) 1))
		     (t
		      nil)))
	     (do-upper-right (r c)
	       (cond ((and (= 0 (b (- r 1) c))
			   (= 0 (b r (+ c 1)))
			   (= 0 (b (- r 1) (+ c 1))))
		      (setf (bit result (* 4 r) (+ (* 4 c) 3)) 0))
		     ((and (= 1 (b (- r 1) c))
			   (= 1 (b r (+ c 1)))
			   (= 1 (b (- r 1) (+ c 1))))
		      (setf (bit result (* 4 r) (+ (* 4 c) 3)) 1))
		     (t
		      nil)))
	     (do-lower-left (r c)
	       (cond ((and (= 0 (b (+ r 1) c))
			   (= 0 (b r (- c 1)))
			   (= 0 (b (+ r 1) (- c 1))))
		      (setf (bit result (+ (* 4 r) 3) (* 4 c)) 0))
		     ((and (= 1 (b (+ r 1) c))
			   (= 1 (b r (- c 1)))
			   (= 1 (b (+ r 1) (- c 1))))
		      (setf (bit result (+ (* 4 r) 3) (* 4 c)) 1))
		     (t
		      nil)))
	     (do-lower-right (r c)
	       (cond ((and (= 0 (b (+ r 1) c))
			   (= 0 (b r (+ c 1)))
			   (= 0 (b (+ r 1) (+ c 1))))
		      (setf (bit result (+ (* 4 r) 3) (+ (* 4 c) 3)) 0))
		     ((and (= 1 (b (+ r 1) c))
			   (= 1 (b r (+ c 1)))
			   (= 1 (b (+ r 1) (+ c 1))))
		      (setf (bit result (+ (* 4 r) 3) (+ (* 4 c) 3)) 1))
		     (t
		      nil))))
	(loop for r from 0 below hh
	      do (loop for c from 0 below ww
		       do (do-upper-left r c)
			  (do-upper-right r c)
			  (do-lower-left r c)
			  (do-lower-right r c)))))
    result))

(defun distance-of-closest-1 (bitmap r c)
  (let ((h (array-dimension bitmap 0))
	(w (array-dimension bitmap 1)))
    (if (and (>= r 0) (< r h) (>= c 0) (< c w)
	     (eql (bit bitmap r c) 1))
	0d0
	(let ((min (+ h w)))
	  (loop for rr from 0 below h
		do (loop for cc from 0 below w
			 do (when (eql (bit bitmap rr cc) 1)
			      (setf min
				    (min min
					 (sqrt (+ (expt (- rr r) 2d0)
						  (expt (- cc c) 2d0))))))))
	  min))))
	     
(defun make-distance-matrix-1 (bitmap)
  (let* ((hh (array-dimension bitmap 0))
	 (hh/2 (floor hh 2))
	 (h (* hh 2))
	 (ww (array-dimension bitmap 1))
	 (ww/2 (floor ww 2))
	 (w (* ww 2)))
    (let ((matrix (make-array (list h w) :element-type 'double-float)))
      (loop for r from 0 below h
	    do (loop for c from 0 below w
		     do (let ((d (distance-of-closest-1
				  bitmap (- r hh/2) (- c ww/2))))
			  (setf (aref matrix r c)
				(expt d 3)))))
      matrix)))

(defun distance-of-closest-0 (bitmap r c)
  (let ((h (array-dimension bitmap 0))
	(w (array-dimension bitmap 1)))
    (if (or (< r 0) (>= r h) (< c 0) (>= c w)
	     (eql (bit bitmap r c) 0))
	0d0
	(let ((min (+ h w)))
	  (loop for rr from 0 below h
		do (loop for cc from 0 below w
			 do (when (eql (bit bitmap rr cc) 0)
			      (setf min
				    (min min
					 (sqrt (+ (expt (- rr r) 2d0)
						  (expt (- cc c) 2d0))))))))
	  min))))

(defun make-distance-matrix-0 (bitmap)
  (let* ((hh (array-dimension bitmap 0))
	 (hh/2 (floor hh 2))
	 (h (* hh 2))
	 (ww (array-dimension bitmap 1))
	 (ww/2 (floor ww 2))
	 (w (* ww 2)))
    (let ((matrix (make-array (list h w) :element-type 'double-float)))
      (loop for r from 0 below h
	    do (loop for c from 0 below w
		     do (let ((d (distance-of-closest-0
				  bitmap (- r hh/2) (- c ww/2))))
			  (setf (aref matrix r c)
				(expt d 3)))))
      matrix)))

(defun distance (bitmap matrix-0 matrix-1 dr dc)
  (declare (type (array bit (* *)) bitmap)
  	   (type (array double-float (* *)) matrix-0 matrix-1)
  	   (type fixnum dr dc))
  (loop for r from 0 below (array-dimension bitmap 0)
	sum (loop for c from 0 below (array-dimension bitmap 1)
		  sum (aref (if (zerop (bit bitmap r c))
				matrix-0
				matrix-1)
			    (+ r dr) (+ c dc)))))

(defun min-distance (bitmap matrix-0 matrix-1)
  (let ((dh (- (array-dimension matrix-0 0)
	       (array-dimension bitmap 0)))
	(dw (- (array-dimension matrix-0 1)
	       (array-dimension bitmap 1))))
    (loop for dr from 0 to dh
	  minimize (loop for dc from 0 to dw
			 minimize (distance bitmap matrix-0 matrix-1 dr dc)))))

(defun max-min-distance (bitmaps matrix-0 matrix-1)
  (print '+++++)
  (loop for bitmap in bitmaps
	maximize (min-distance bitmap matrix-0 matrix-1)))

(defun best-representative (bitmaps)
  (let ((best nil)
	(min-max-min-distance 1d10))
    (loop for bitmap in bitmaps
	  do (let ((m0 (make-distance-matrix-0 bitmap))
		   (m1 (make-distance-matrix-1 bitmap)))
	       (let ((d (max-min-distance (remove bitmap bitmaps) m0 m1)))
		 (when (< d min-max-min-distance)
		   (setf min-max-min-distance d)
		   (setf best bitmap)))))
    (values best min-max-min-distance)))

(defun best-offset (bitmap matrix-0 matrix-1)
  (let ((best-distance 1d10)
	(best-dr nil)
	(best-dc nil)
	(dh (- (array-dimension matrix-0 0)
	       (array-dimension bitmap 0)))
	(dw (- (array-dimension matrix-0 1)
	       (array-dimension bitmap 1))))
    (loop for dr from 0 to dh
	  do (loop for dc from 0 to dw
		   do (let ((d (distance bitmap matrix-0 matrix-1 dr dc)))
			(when (< d best-distance)
			  (setf best-distance d)
			  (setf best-dr dr)
			  (setf best-dc dc)))))
    (values best-dr best-dc)))

(defun add-bitmap-to-total (bitmap total dr dc)
  (print '---)
  (loop for r from 0 below (array-dimension bitmap 0)
	do (loop for c from 0 below (array-dimension bitmap 1)
		 do (incf (aref total (+ r dr) (+ c dc))
			  (bit bitmap r c)))))

(defun compute-total (bitmaps)
  (let* ((best (best-representative bitmaps))
	 (m0 (make-distance-matrix-0 best))
	 (m1 (make-distance-matrix-1 best))
	 (total (make-array (array-dimensions m0)
			    :element-type 'integer
			    :initial-element 0)))
    (loop for bitmap in bitmaps
	  do (multiple-value-bind (dr dc) (best-offset bitmap m0 m1)
	       (add-bitmap-to-total bitmap total dr dc)))
    total))

(defun total-to-pbm (total filename)
  (let* ((h (array-dimension total 0))
	 (w (array-dimension total 1))
	 (max (loop for r from 0 below h
		    maximize (loop for c from 0 below w
				   maximize (aref total r c)))))
    (with-open-file (stream
		     filename
		     :direction :output
		     :if-exists :supersede)
      (format stream "P2~%~a ~a~% ~a~%" w h max)
      (loop for r from 0 below h
	    do (loop for c from 0 below w
		     do (format stream "~a " (- max (aref total r c))))
	       (format stream "~%")))))
