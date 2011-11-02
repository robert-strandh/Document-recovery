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

(defclass glyph ()
  ((%image :initarg :image :reader image)))

(defclass glyph-group ()
  ((%name :initarg :name :accessor name)
   (%instances :initarg :instances :initform '() :accessor instances)))

(defclass font ()
  ((%name :initarg :name :accessor name)
   (%glyph-groups :initform '() :initarg :glyph-groups :accessor glyph-groups)))

(define-application-frame stuff ()
  ((%current-image :initform nil :accessor current-image)
   (%fonts :initform '() :accessor fonts)
   (%current-font :initform '() :accessor current-font)
   (%current-glyph-group :initform nil :accessor :current-glyph-group))
  (:panes
   (app :application :width 1100 :height 4500 :display-time nil :scroll-bars nil)
   (files :application :width 200 :max-width 200 :height 10000 :display-time nil :scroll-bars nil)
   (fonts :application :width 100 :max-widht 100 :height 1000 :scroll-bars nil)
   (current-font :application :width 100 :max-widht 100 :height 1000 :scroll-bars nil)
   (current-glyph-group :application :width 100 :max-widht 100 :height 1000 :scroll-bars nil)
   (int :interactor :width 1100 :height 100 :max-height 100 :scrollbars t))
  (:layouts
   (default (vertically ()
	      (horizontally ()
		(scrolling (:width 1100 :min-width 1100 :height 1000 :scroll-bars t) app)
		(scrolling (:width 100 :max-width 100 :scroll-bars t) files)
		(scrolling (:width 100 :max-width 100 :scroll-bars t) fonts)
		(scrolling (:width 100 :max-width 100 :scroll-bars t) current-font)
		(scrolling (:width 100 :max-width 100 :scroll-bars t) current-glyph-group))
	      int))))

(defun stuff ()
  (run-frame-top-level (make-application-frame 'stuff)))

(define-stuff-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(defun convert-image (image)
  (loop with new = (make-array (array-dimensions image) :element-type '(unsigned-byte 32))
	for row from 0 below (array-dimension image 0)
	do (loop for col from 0 below (array-dimension image 1)
		 do (let ((pixel (aref image row col)))
		      (setf (aref new row col)
			    (+ (ash pixel 16) (ash pixel 8) pixel))))
	finally (return new)))

(defun display-converted-image (converted-image)
  (let ((pane (find-pane-named *application-frame* 'app)))
    (window-clear pane)
    (let ((record (make-instance 'client-rgb-image-record :array converted-image)))
      (add-output-record record (stream-output-history pane))
      (replay record pane))))

(define-stuff-command (com-read-page :name t) ((filename 'string))
  (let ((image (image::read-image-file filename)))
    (display-converted-image (convert-image image))))

(define-stuff-command (com-define-area :name t) ()
  (let ((n 0)
	(sheet (find-pane-named *application-frame* 'app))
	p1 p2 p3)
    (block track
      (tracking-pointer (sheet)
        (:pointer-button-press (&key event x y)
	  (declare (ignore event))
	  (cond ((zerop n)
		 (setf p1 (make-point x y))
		 (incf n))
		((= n 1)
		 (setf p2 (make-point x y))
		 (draw-line sheet p1 p2 :ink +red+)
		 (incf n))
		((= n 2)
		 (setf p3 (make-point x y))
		 (draw-line sheet p2 p3 :ink +red+)
		 (return-from track nil))))))))

(define-stuff-command (com-define-rectangle :name t) ()
  (let ((n 0)
	(sheet (find-pane-named *application-frame* 'app))
	p1 p2)
    (block track
      (tracking-pointer (sheet)
        (:pointer-button-press (&key event x y)
	  (declare (ignore event))
	  (cond ((zerop n)
		 (setf p1 (make-point x y))
		 (incf n))
		((= n 1)
		 (setf p2 (make-point x y))
		 (draw-rectangle sheet p1 p2 :filled nil :ink +red+)
		 (return-from track nil))))))))

(defclass page ()
  ((%filename :initarg :filename :reader filename)
   (%image :initarg :image :initform nil :accessor image)))

(defmethod print-object ((page page) stream)
  (format stream "[~a]" (file-namestring (filename page))))

(define-stuff-command (com-show-files :name t) ()
  (loop with pane = (find-pane-named *application-frame* 'files)
	for file in (directory "???-?.pgm")
	do (with-output-as-presentation  (pane (make-instance 'page :filename file) 'page)
	     (format pane "~a~%" (file-namestring file)))))

(define-stuff-command (com-display-image :name t) ((page 'page))
  (when (null (image page))
    (setf (image page) (image:read-image-file (filename page))))
  (display-converted-image (convert-image (image page))))

(define-presentation-to-command-translator display-image
    (page com-display-image stuff)
  (object)
  `(,object))
