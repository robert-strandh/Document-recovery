(cl:in-package #:salvager-slope)

(defun straighten (picture)
  (let ((slope (determine-slope picture)))
    (rotate-picture picture slope)))
