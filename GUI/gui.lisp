(cl:in-package #:salvager-gui)

(clim:define-application-frame salvager ()
  ((%current-image :initform nil :accessor current-image)
   (%current-pattern :initform nil :accessor current-pattern)
   (%current-directory
    :initarg :current-directory
    :accessor current-directory))
  (:panes
   (image :application :width 1100 :height 3000
                       :scroll-bars nil
                       :display-function 'display-image)
   (interactor :interactor :width 1100 :height 100 :max-height 100
                           :scroll-bars nil))
  (:layouts
   (default (clim:vertically ()
              (clim:scrolling
                  (:width 1100 :min-width 1100
                   :height 1000 :scroll-bars t)
                image)
              (clim:scrolling
                  (:width 100 :min-width 1100
                   :scroll-bars t)
                interactor)))))

(defun display-image (frame pane)
  (declare (ignore frame))
  (let ((pattern (current-pattern clim:*application-frame*)))
    (unless (null pattern)
      (let* ((transformation (clim:make-scaling-transformation 2 2))
             (transformed-pattern
               (clim:transform-region transformation pattern)))
        (clim:draw-pattern* pane transformed-pattern 0 0)))))

(defun salvager ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'salvager
     :current-directory *default-pathname-defaults*)))

(define-salvager-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-salvager-command (com-read-file :name t)
    ((filename
      'pathname
      :default (namestring (current-directory clim:*application-frame*))
      :default-type 'pathname
      :insert-default t))
  (with-open-file (stream filename :direction :input)
    (let ((image (opticl:read-pgm-file filename)))
      (setf (current-image clim:*application-frame*) image)
      (setf (current-pattern clim:*application-frame*)
            (clim:make-pattern
             image
             (loop for i below 256
                   collect (clim:make-gray-color (/ i 255))))))))
