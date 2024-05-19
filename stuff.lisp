(defun compute-histogram (array)
  (let ((histogram (make-array 256 :initial-element 0)))
    (loop for row from 0 below (array-dimension array 0)
	  do (loop for col from 0 below (array-dimension array 1)
		   do (incf (aref histogram (aref array row col)))))
    histogram))

;;; We define a frontier k in a histogram h and consider the two
;;; halves l and r of the histogram defined by entries 0..k and
;;; k+1..n-1 where n is the number of entries in the histogram,
;;; typically 256.  For each half we keep values a, b, and c, where a
;;; is the sum of the entries, b is the sum of each entry multiplied
;;; by its index, and c is the sum of each entry multiplied by the
;;; square of its index.  Thus:
;;;
;;;   al = sum(i=0,k) h[i]
;;;   bl = sum(i=0,k) i*h[i]
;;;   cl = sum(i=0,k) i^2*h[i]
;;;   ar = sum(i=k+1,n-1) h[i]
;;;   br = sum(i=k+1,n-1) i*h[i]
;;;   cr = sum(i=k+1,n-1) i^2*h[i]
;;;
;;; From these values, we can compute the wieighted variance v of the
;;; values represented by each half of the histogram:
;;;
;;;   vl = (cl^2 - bl^2)/al
;;;   vr = (cr^2 - br^2)/ar
;;;
;;; We compute vl and vr for all values of k such that both al and ar
;;; are strictly positive.  The best value of k is the one that
;;; minimizes vl + vr, i.e. the sum of the weighted differences of the
;;; variances.
(defun best-frontier (histogram)
  (let ((length (length histogram)))
    (let ((k -1)
	  (al 0)
	  (bl 0)
	  (cl 0)
	  (ar (loop for i from 0 below length
		    sum (aref histogram i)))
	  (br (loop for i from 0 below length
		    sum (* i (aref histogram i))))
	  (cr (loop for i from 0 below length
		    sum (* i i (aref histogram i))))
	  best-frontier
	  best-val)
      ;; We assume the histogram contains at least two non-zero
      ;; entries.
      (flet ((one-step ()
	       (incf k)
	       (incf al (aref histogram k))
	       (incf bl (* k (aref histogram k)))
	       (incf cl (* k k (aref histogram k)))
	       (decf ar (aref histogram k))
	       (decf br (* k (aref histogram k)))
	       (decf cr (* k k (aref histogram k))))
	     (compute-variance (a b c)
	       (- c (/ (* b b) a))))
	(loop until (plusp al)
	      do (one-step))
	(setf best-val
	      (+ (compute-variance al bl cl)
		 (compute-variance ar br cr)))
	(setf best-frontier k)
	(loop do (one-step)
	      until (zerop ar)
	      do (let ((val (+ (compute-variance al bl cl)
			       (compute-variance ar br cr))))
		   (when (< val best-val)
		     (setf best-val val
			   best-frontier k)))))
      best-frontier)))
	
;;; For a column of the array, binarize the column and return
;;; the result in the form of a list of intevals of black, 
;;; i.e. intervals where the value is less than or equal to
;;; the threshold.
(defun compute-column-intervals (array column threshold)
  (loop with white-p = t
	with result = '()
	for row from (1- (array-dimension array 0)) downto 0
	do (if white-p
	       (when (<= (aref array row column) threshold)
		 ;; start a new interval
		 (push (cons row (1+ row)) result)
		 (setf white-p nil))
	       (if (> (aref array row column) threshold)
		   (setf white-p t)
		   (decf (caar result))))
	finally (return result)))

(defun binarize (array)
  (loop with threshold = (1+ (best-frontier (compute-histogram array)))
	with width = (array-dimension array 1)
	with columns = (make-array width)
	for col from 0 below width
	do (setf (aref columns col)
		 (compute-column-intervals array col threshold))
	finally (return columns)))

;;; For testing purposes

(defun histogram-variance (histogram start end)
  (let* ((n (loop for i from start below end
		  sum (aref histogram i)))
	 (avg (/ (loop for i from start below end
		       sum (* i (aref histogram i)))
		 n)))
    (/ (loop for i from start below end
	     sum (* (aref histogram i)
		    (- i avg)
		    (- i avg)))
       n)))
  
(defun histogram-mumble (histogram start end)
  (let* ((n (loop for i from start below end
		  sum (aref histogram i)))
	 (avg (/ (loop for i from start below end
		       sum (* i (aref histogram i)))
		 n)))
    (loop for i from start below end
	  sum (* (aref histogram i)
		 (- i avg)
		 (- i avg)))))

(defun stupid-diff (histogram frontier)
  (let ((vl (histogram-variance histogram 0 (1+ frontier)))
	(vr (histogram-variance histogram (1+ frontier) 256)))
    (abs (- vl vr))))
	   
(defun stupid-variance (list)
  (let ((average (/ (reduce #'+ list) (length list))))
    (/ (reduce #'+ list :key (lambda (x) (expt (- x average) 2)))
       (length list))))

(defun test-variance ()
  (let* ((list (list* 1 255 (loop repeat 100000 collect (random 256))))
	 (histogram (make-array 256 :initial-element 0)))
    (loop for element in list
	  do (incf (aref histogram element)))
    (loop for frontier from 1 to 254
	  do (assert (= (stupid-variance (remove-if-not (lambda (x) (<= x frontier))
							list))
			(histogram-variance histogram 0 (1+ frontier))))
	  do (assert (= (stupid-variance (remove-if-not (lambda (x) (> x frontier))
							list))
			(histogram-variance histogram (1+ frontier) 256))))))
