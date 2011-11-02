(defun bound (page char dr dc)
  (let ((h (array-dimension char 0))
	(w (array-dimension char 1)))
    (let ((averagep (/ (loop for r from 0 below h
			     sum (loop for c from 0 below w
				       sum (aref page (+ r dr) (+ c dc))))
		       (* h w)))
	  (averagec (/ (loop for r from 0 below h
			     sum (loop for c from 0 below w
				       sum (aref char r c)))
		       (* h w))))
      (let ((a (loop for r from 0 below h
		     sum (loop for c from 0 below w
			       sum (abs (- (aref page (+ r dr) (+ c dc))
					   averagep)))))
	    (b (loop for r from 0 below h
		     sum (loop for c from 0 below w
			       sum (let ((d (- (aref page (+ r dr) (+ c dc))
					       averagec)))
				     (* d d))))))
	(/ a (sqrt (* h w b)))))))

(defun corr (page char dr dc)
  (let ((h (array-dimension char 0))
	(w (array-dimension char 1)))
    (let ((averagep (/ (loop for r from 0 below h
			     sum (loop for c from 0 below w
				       sum (aref page (+ r dr) (+ c dc))))
		       (* h w)))
	  (averagec (/ (loop for r from 0 below h
			     sum (loop for c from 0 below w
				       sum (aref char r c)))
		       (* h w))))
      (let ((a (loop for r from 0 below h
		     sum (loop for c from 0 below w
			       sum (if (> (aref char r c) 128)
				       (- (aref page (+ r dr) (+ c dc)) 
					  averagep)
				       (- averagep 
					  (aref page (+ r dr) (+ c dc)))))))
	    (b (loop for r from 0 below h
		     sum (loop for c from 0 below w
			       sum (let ((d (- (aref page (+ r dr) (+ c dc))
					       averagec)))
				     (* d d))))))
	(/ a (sqrt (* h w b)))))))

(defun corr2 (page char dr dc)
  (let ((h (array-dimension char 0))
	(w (array-dimension char 1)))
    (let* ((averagep (/ (loop for r from 0 below h
			      sum (loop for c from 0 below w
					sum (aref page (+ r dr) (+ c dc))))
			(* h w)))
	   (averagec (/ (loop for r from 0 below h
			      sum (loop for c from 0 below w
					sum (aref char r c)))
			(* h w)))
	   (c (loop for r from 0 below h
		    sum (loop for c from 0 below w
			      sum (let ((x (- (aref char r c) averagec)))
				    (* x x))))))
      (let ((a (loop for r from 0 below h
		     sum (loop for c from 0 below w
			       sum (* (- (aref char r c) averagec)
				      (- (aref page (+ r dr) (+ c dc)) averagep)))))
	    (b (loop for r from 0 below h
		     sum (loop for c from 0 below w
			       sum (let ((d (- (aref page (+ r dr) (+ c dc))
					       averagec)))
				     (* d d))))))
	(/ a (sqrt (* c b)))))))

(defun tt ()
  (let ((a (make-array 5))
	(h (make-array 20 :initial-element 0)))
    (let ((sum (loop for i from 0 below 5
		     do (setf (aref a i) (random 20))
		     do (incf (aref h (aref a i)))
		     sum (aref a i))))
      (let* ((average (/ sum 5))
	     ;; left contains SUM h(i) s.t. i < average
	     (left (loop for i from 0
			 while (< i average)
			 sum (aref h i))))
	(let ((acc (loop for i from 0 below 5
			 sum (abs (- (aref a i) average)))))
	  (loop repeat 10
		do (progn (format *trace-output*
				  "    a: ~s~%    h: ~s~%    sum ~s  av: ~s  acc: ~s  left: ~s~%"
				  a h sum average acc left)
			  (finish-output *trace-output*))
		do (let ((random-value (random 20))
			 (random-index (random 5)))
		     (format *trace-output*
			     "random-value: ~s  random-index: ~s~%"
			     random-value random-index)
		     (finish-output *trace-output*)
		     (decf sum (aref a random-index))
		     (incf sum random-value)
		     ;; modify the histogram
		     (decf (aref h (aref a random-index)))
		     (incf (aref h random-value))
		     (if (< (aref a random-index) average)
			 (progn (decf acc (- average (aref a random-index)))
				(decf left))
			 (decf acc (- (aref a random-index) average)))
		     (if (< random-value average)
			 (progn (incf acc (- average random-value))
				(incf left))
			 (incf acc (- random-value average)))
		     ;; adjust the average
		     (let ((new-average (/ sum 5)))
		       (if (< new-average average)
			   (loop for i from (ceiling new-average) below average
				 do (incf acc (* 2 (aref h i) (- average i)))
				 do (decf left (aref h i)))
			   (loop for i from (ceiling average) below new-average
				 do (incf acc (* 2 (aref h i) (- average i)))
				 do (incf left (aref h i))))
		       ;; acc now contains
		       ;; SUM | a[i] - average |
		       ;; Check that it does
		       (let ((acc-should (loop for i from 0 below 5
					       sum (abs (- (aref a i) average)))))
			 (when (/= acc acc-should)
			   (format *trace-output*
				 "acc should contain: ~s but contains: ~s~%"
				 acc-should acc)))
		       ;; and we need to turn that into
		       ;; SUM | a[i] - new-average | 
		       (incf acc (* left (- new-average average)))
		       (decf acc (* (- 5 left) (- average new-average)))
		       (setf average new-average))
		     (setf (aref a random-index) random-value)
		     (let ((acc-should-be (loop for i from 0 below 5
						sum (abs (- (aref a i) average)))))
		       (format *trace-output*
			       "new a: ~s~%new h: ~s~%new sum ~s  av: ~s  acc: ~s  left: ~s~%"
			       a h sum average acc left)
		       (format *trace-output*
			       "acc shuld be: ~s~%" acc-should-be)
		       (finish-output *trace-output*)
		       (assert (= acc acc-should-be))))))))))

(defun level (page h w sum)
  (let ((ph (array-dimension page 0))
	(pw (array-dimension page 1))
	(slack (* 10 h w))
	(occurrences '()))
    (let ((sums (make-array pw)))
      (loop for c from 0 below pw
	    do (setf (aref sums c)
		     (loop for r from 0 below h
			   sum (aref page r c))))
      (loop for r from 0 below (- ph h)
	    do (let ((total (loop for c from 0 below w
				  sum (aref sums c))))
		 (loop for c from 0 below (- pw w)
		       do (when (< (- sum slack) total (+ sum slack))
			    (let ((corr (corr2 page *c* r c)))
			      (if (> corr 0.5)
				  (progn 
				    (format *trace-output*
					    "r: ~s  c: ~s  corr: ~s~%"
					    r c corr)
				    (finish-output *trace-output*)
				    (push (list corr r c) occurrences))
				  (progn
				    (format *trace-output*
					    "****r: ~s  c: ~s  corr: ~s~%"
					    r c corr)
				    (finish-output *trace-output*)))))
		       do (decf total (aref sums c))
		       do (incf total (aref sums (+ c w)))))
	    do (loop for c from 0 below pw
		     do (decf (aref sums c) (aref page r c))
		     do (incf (aref sums c) (aref page (+ r h) c)))))
    (setf occurrences (sort occurrences #'> :key #'car))
    (loop until (null occurrences)
	  collect (let ((first (pop occurrences)))
		    (setf occurrences
			  (remove-if (lambda (occurrence)
				       (and (< (abs (- (second first) (second occurrence)))
					       (* 0.5 h))
					    (< (abs (- (third first) (third occurrence)))
					       (* 0.5 w))))
				     occurrences))
		    first))))

(defun level2 (page char)
  (let* ((ph (array-dimension page 0))
	 (pw (array-dimension page 1))
	 (h (array-dimension char 0))
	 (w (array-dimension char 1))
	 (h/2 (floor h 2))
	 (w/2 (floor w 2))
	 (sum-ul (loop for r from 0 below h/2
		       sum (loop for c from 0 below w/2
				 sum (aref char r c))))
	 (sum-ur (loop for r from 0 below h/2
		       sum (loop for c from w/2 below w
				 sum (aref char r c))))
	 (sum-ll (loop for r from h/2 below h
		       sum (loop for c from 0 below w/2
				 sum (aref char r c))))
	 (sum-lr (loop for r from h/2 below h
		       sum (loop for c from w/2 below w
				 sum (aref char r c))))
	 (sum (+ sum-ul sum-ur sum-ll sum-lr)))
    (let ((acc (let ((acc (make-array (list (1+ ph) (1+ pw)) :initial-element 0)))
		 (loop for r from 1 to ph
		       do (loop for c from 1 to pw
				do (setf (aref acc r c)
					 (- (+ (aref acc (1- r) c)
					       (aref acc r (1- c))
					       (aref page (1- r) (1- c)))
					    (aref acc (1- r) (1- c))))))
		 acc))
	  (slack (* 10 h w))
	  (occurrences '()))
      (flet ((total (r1 c1 r2 c2)
	       (- (+ (aref acc r2 c2)
		     (aref acc r1 c1))
		  (+ (aref acc r2 c1)
		     (aref acc r1 c2)))))
	(loop for r from 0 below (- ph h)
	      do (loop for c from 0 below (- pw w)
		       do (let ((total (total r c (+ r h) (+ c w))))
			    (when (< (- sum slack) total (+ sum slack))
			      (let ((slack/4 (floor slack 2)))
				(when (and (< (- sum-ul slack/4)
					      (total r c (+ r h/2) (+ c w/2))
					      (+ sum-ul slack/4))
					   (< (- sum-ur slack/4)
					      (total r (+ c w/2) (+ r h/2) (+ c w))
					      (+ sum-ur slack/4))
					   (< (- sum-ll slack/4)
					      (total (+ r h/2) c (+ r h) (+ c w/2))
					      (+ sum-ll slack/4))
					   (< (- sum-lr slack/4)
					      (total (+ r h/2) (+ c w/2) (+ r h) (+ c w))
					      (+ sum-lr slack/4)))
				  (let ((corr (corr2 page *c* r c)))
				    (when (> corr 0.5)
				      (push (list corr r c) occurrences))))))))))
      (setf occurrences (sort occurrences #'> :key #'car))
      (sort (loop until (null occurrences)
		  collect (let ((first (pop occurrences)))
			    (setf occurrences
				  (remove-if (lambda (occurrence)
					       (and (< (abs (- (second first) (second occurrence)))
						       (* 0.5 h))
						    (< (abs (- (third first) (third occurrence)))
						       (* 0.5 w))))
					     occurrences))
			    first))
	    #'<
	    :key #'second))))