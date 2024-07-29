(cl:in-package #:salvager-slope)

;;;; The slope is computed as a (hopefully small) rational number that
;;;; can be thought of as the tangent of the angle that the picture is
;;;; rotated, where a positive slope means rotation of a negative angle

;;;; FIXME: maybe change this convention.

;;;; Since the angle is small, the tangent is roughly equal to the
;;;; angle itself, the sine of the angle is also roughly equal to the
;;;; angle itself, and the cosine of the angle is roughly equal to 1.

(defun compute-energy (vector)
  (reduce #'+ vector :key (lambda (x) (* x x))))

(defun sum-column (picture column row-sums offset)
  (let ((height (array-dimension picture 0)))
    (if (minusp offset)
        (progn (loop for i from 0 below (- offset)
                     do (incf (aref row-sums i) 255))
               (loop for i from (- offset) below height
                     do (incf (aref row-sums i)
                              (aref picture (+ i offset) column))))
        (progn (loop for i from 0 below (- height offset)
                     do (incf (aref row-sums i)
                              (aref picture (+ i offset) column)))
               (loop for i from (- height offset) below height
                     do (incf (aref row-sums i) 255))))))

(defun slope-energy (picture slope)
  (let* ((height (array-dimension picture 0))
         (width (array-dimension picture 1))
         (row-sums (make-array height :initial-element 0)))
    (loop for column from 0 below width
          for offset = (round (* column slope))
          do (sum-column picture column row-sums offset))
    (compute-energy row-sums)))

(defun determine-slope (picture)
  (let ((width (array-dimension picture 1))
        (max-energy 0)
        (slope-with-max-energy 0))
    (loop for delta from -100 to 100
          for slope = (/ delta width)
          for energy = (slope-energy picture slope)
          do (when (> energy max-energy)
               (setf max-energy energy)
               (setf slope-with-max-energy slope)))
    slope-with-max-energy))
