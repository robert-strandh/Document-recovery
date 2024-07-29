(defparameter *count* 0)

;;; Take two slopes as rational numbers and compute the vertical
;;; offset for each one at every horizontal point between 0 and WIDTH.
;;; Round the vertical offset for each slope to an integer.  If the
;;; two rounded vertical slopes are not the same, call FUNCTION with
;;; the horizontal position and the two rounded vertical offsets.
(defun mumble (old-slope new-slope width function)
  (loop for horizontal-position from 0 below width
        for old-vertical-offset = (round (* horizontal-position old-slope))
        for new-vertical-offset = (round (* horizontal-position new-slope))
        unless (= old-vertical-offset new-vertical-offset)
          do (funcall function
                      horizontal-position
                      old-vertical-offset
                      new-vertical-offset)))

(defun next-slope (slope width)
  (when (> (incf *count*) 1000)
    (setf *count* 0)
    (format *trace-output* "~a~%" (float slope)))
  (loop for column from 1 below width
	minimize (/ (floor (1+ (* slope column))) column)))

(defun move-value (value vector from to)
  (let ((length (length vector)))
    (let ((from (mod (+ from length) length))
	  (to (mod (+ to length) length)))
      (decf (aref vector from) value)
      (incf (aref vector to) value))))

(defun compute-slope (array)
  (let ((height (array-dimension array 0))
	(width (array-dimension array 1)))
    (let ((column-sums (make-array height))
	  (slope 0)
	  (max-energy-slope 0)
	  max-energy)
      ;; compute initial energy
      (loop for r from 0 below height
	    do (setf (aref column-sums r)
		     (loop for c from 0 below width
			   sum (aref array r c))))
      (setf max-energy (compute-energy column-sums))
      (loop while (< slope 1/5)
	    do (setf slope (next-slope slope width))
	    do (loop for column from 1 below width
		     do (let ((delta (* slope column)))
			  (when (integerp delta)
			    (loop for row from 0 below height
				  do (move-value
				      (aref array row column)
				      column-sums
				      (1- (- row delta))
				      (- row delta))))))
	    do (let ((new-energy (compute-energy column-sums)))
		 (when (> new-energy max-energy)
		   (setf max-energy new-energy
			 max-energy-slope slope))))
      max-energy-slope)))
