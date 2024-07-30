(cl:in-package #:salvager-gui)

(clim:define-application-frame salvager ()
  ((%current-image :initform nil :accessor current-image))
  (:panes
   (image :application :width 1100 :height 4500
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
                  (:width 100 :max-width 100
                   :scroll-bars t)
                interactor)))))

(defun display-image (frame pane)
  (declare (ignore frame pane)))

(defun salvager ()
  (clim:run-frame-top-level (clim:make-application-frame 'salvager)))

(define-salvager-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))
