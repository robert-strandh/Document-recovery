(in-package :clim-user)

(define-application-frame stuff ()
  ((%current-image :initform nil :accessor current-image))
  (:panes
   (image :application :width 1100 :height 4500
                       :scroll-bars nil
                       :display-function 'display-image)
   (interactor :interactor :width 1100 :height 100 :max-height 100
                           :scrollbars nil))
  (:layouts
   (default (vertically ()
              (horizontally ()
                (scrolling
                    (:width 1100 :min-width 1100
                     :height 1000 :scroll-bars t)
                  image)
                (scrolling (:width 100 :max-width 100 :scroll-bars t) files))
              interactor))))

(defun stuff ()
  (run-frame-top-level (make-application-frame 'stuff)))

(define-stuff-command (com-quit :name t) ()
  (frame-exit *application-frame*))

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

(define-stuff-command (com-display-image :name t) ((page 'page))
  (when (null (image page))
    (setf (image page) (image:read-image-file (filename page))))
  (display-converted-image (convert-image (image page))))

(define-presentation-to-command-translator display-image
    (page com-display-image stuff)
  (object)
  `(,object))
