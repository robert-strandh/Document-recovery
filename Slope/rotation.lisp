(cl:in-package #:salvager-slope)

;;;; Once we have determined the slope of the picture, we rotate it so
;;;; that its slope is 0.  We rotate it around the pixel closes to the
;;;; middle of the pixture.  We do this by creating a new picture with
;;;; the same size as the original, and for each pixel in the new
;;;; picture, we determine the value by a weigted average of the four
;;;; closest corresponding pixels in the original image.
;;;;
;;;; Call the pixel around which rotation is to take place (x0,y0).
;;;; For an arbitrary pixel (x,y) in the new image, we compute two
;;;; (usually non-integer) coordinates in the original image as
;;;; follows.  First, we determine the point (dx,dy) as
;;;; (x-x0,y-y0). Then we transform (dx,dy) into (dx1,dy1) by applying
;;;; a rotation transformation with the angle a to it.  This way we
;;;; get dx1 = dx*cos a - dy*sin a and dy1 = dy*cos a + dx*sin a.
;;;;
;;;; But a is small, so we can approximage cos a by 1 and sin a by a.
;;;; We get dx1 = dx - dy*a and dy1 = dy + dx*a.
;;;; 
;;;; Now, (dx1,dy1) is relative to (x0,y0), so we compute (x1,y1) as
;;;; (dx1+x0,dy1+y0).  
;;;;
;;;; We then express x1 as X1 + epsilon and y1 as Y1 + delta, where X1
;;;; and Y1 are integers.  We compute a weighted average between the
;;;; pixels (X1,Y1), (X1+1,Y1), (X1,Y1+1) and (X1+1,Y1+1) in the
;;;; original imagine as follows (where V is the value of the pixel):
;;;; V(X1,Y1)*(1-epsilon)*(1-delta) + V(X1+1,Y1)*epsilon*(1-delta) +
;;;; V(X1,Y1+1)*(1-epsilon)*delta + V(X1+1,Y1+1)*epsilon*delta.

(defun weighted-average (picture x1 y1)
  (multiple-value-bind (X1 epsilon) (floor x1)
    (multiple-value-bind (Y1 delta) (floor y1)
      (round (+ (* (aref picture Y1 X1) (- 1 epsilon) (- 1 delta))
                (* (aref picture Y1 (1+ X1)) epsilon (- 1 delta))
                (* (aref picture (1+ Y1) X1) (- 1 epsilon) delta)
                (* (aref picture (1+ Y1) (1+ X1)) epsilon delta))))))

(defun pixel-in-original-picture (x0 y0 x y angle)
  (let* ((dx (- x x0))
         (dy (- y y0))
         (dx1 (- dx (* dy angle)))
         (dy1 (+ dy (* dx angle))))
    (values (+ dx1 x0) (+ dy1 y0))))

(defun new-pixel-value (original-picture x0 y0 x y angle)
  (multiple-value-bind (x1 y1)
      (pixel-in-original-picture x0 y0 x y angle)
    (if (or (< x1 0) (> x1 (- (array-dimension original-picture 1) 2))
            (< y1 0) (> y1 (- (array-dimension original-picture 0) 2)))
        255
        (weighted-average original-picture x1 y1))))

(defun rotate-picture (original-picture angle)
  (let* ((height (array-dimension original-picture 0))
         (width (array-dimension original-picture 1))
         (middle-row (floor height 2))
         (middle-column (floor width 2))
         (new-picture (make-array (list height width)
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 255)))
    (loop for y from 0 below height
          do (loop for x from 0 below width
                   do (setf (aref new-picture y x)
                            (new-pixel-value original-picture
                                             middle-column middle-row
                                             x y angle))))
    new-picture))
                                             
