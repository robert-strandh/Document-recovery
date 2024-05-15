(in-package :clim-user)

(defmethod clim::medium-draw-pixels* 
    ((medium clim-clx::clx-medium) array x y &key &allow-other-keys)
  (let* ((width  (array-dimension array 1))
         (height (array-dimension array 0))
         (image (xlib:create-image :width width 
                                   :height height
                                   :data array
                                   :bits-per-pixel 32
                                   :depth 24
                                   :format :z-pixmap)))
    (clim-clx::with-clx-graphics (medium)
      (xlib:put-image clim-clx::mirror clim-clx::gc image :x x :y y :width width :height height))))

(defclass client-rgb-image-record (displayed-output-record)
  ((array  :initarg :array)
   (parent :initarg :parent :accessor output-record-parent :initform nil)
   (x      :initarg :x :initform 0)
   (y      :initarg :y :initform 0)))

(defmethod output-record-position ((record client-rgb-image-record))
  (with-slots (x y) record 
    (values x y)))

(defmethod replay-output-record ((record client-rgb-image-record) stream
                                 &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (with-slots (array) record
    (multiple-value-call #'clim::medium-draw-pixels*
      (sheet-medium stream) array
      (output-record-position record))))

(defmethod bounding-rectangle* ((record client-rgb-image-record))
  (with-slots (array) record
    (multiple-value-bind (x y) (output-record-position record)
      (values x y (+ x (array-dimension array 1)) (+ y (array-dimension array 0))))))
