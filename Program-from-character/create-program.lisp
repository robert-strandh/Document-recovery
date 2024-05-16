;;;; For the purpose of this code, a "character" is a 2-dimensional
;;;; bitmap, so a pixel of the character is either black or white.
;;;; This restriction will be relaxed later so that an element of a
;;;; character can be either a black pixel, a white pixel, or a "not a
;;;; member" element.  The character is used to compute a
;;;; "correlation" between it and an area of the same size of a text
;;;; represented as a 2-dimensional array of gray-scale values between
;;;; 0 and 255.  We consider the correlation between a pixel P (so
;;;; either black or white) in the character and a pixel Q in the text
;;;; to be Q if P is white and -Q if P is black.  The total
;;;; correlation between the character and the area of the text is
;;;; then simply the sum of each individual pixel correlation.  Once
;;;; we introduce the "not a member" element, the corresponding value
;;;; of Q will simply not be taken into account when P is such a
;;;; member.

(defun create-row-program (character row)
  (flet ((a (c) (aref character row c)))
    (flet ((blackp (c) (<= (a c) 128))
           (whitep (c) (> (a c) 128)))
      (let* ((width (array-dimension character 1))
             (last (1- width)))
        (append `((,(if (whitep 0) 'subtract 'add) 0)) 
                (loop for c below (1- width)
                      when (and (whitep c) (blackp (1+ c)))
                        collect `(add ,(1+ c))
                        and collect `(add ,(1+ c))
                      when (and (blackp c) (whitep (1+ c)))
                        collect `(subtract ,(1+ c))
                        and collect `(subtract ,(1+ c)))
                `((,(if (whitep last) 'add 'subtract) ,(1+ last))))))))

;;; This function turns a 2-dimensional array of gray values into the
;;; code of a program.  The resulting program takes as arguments a
;;; 2-dimensional array, representing some page of text, and a row and
;;; a column index of that array.  The program computes the difference
;;; in "correlation" between the character and the page of text at the
;;; position of the row and the column, and the character and the page
;;; of text one position to the right, i.e. at row and column+1.
(defun create-program (character)
  `(lambda (array row column)
     (let ((value 0))
       ,@(loop for r below (array-dimension character 0)
               collect
               `(flet ((add (c)
                         (incf value
                               (aref array (+ row ,r) (+ column c))))
                       (subtract (c)
                         (decf value
                               (aref array (+ row ,r) (+ column c)))))
                  ,@(create-row-program character r)))
       value)))

;;; This function takes a character represented as a 2-dimensional
;;; array of gra-scale pixels, a text, also represented as a
;;; 2-dimensional array of gray-scale pixels, and a position in the
;;; text, and it computes the correlation (as defined above) between
;;; the character and the text at that position.  For this purpose, a
;;; pixel of the character is considered black if its gray-level is
;;; less than or equal to 128 and white otherwise.
(defun initial-value (character array row column)
  (flet ((a (r c) (aref character r c)))
    (flet (#+(or)(blackp (r c) (<= (a r c) 128))
             (whitep (r c) (> (a r c) 128)))
      (destructuring-bind (height width)
          (array-dimensions character)
        (loop with value = 0
              for r below height
              do (loop for c below width
                       do (if (whitep r c)
                              (incf value (aref array (+ row r) (+ column c)))
                              (decf value (aref array (+ row r) (+ column c)))))
              finally (return value))))))

(defun doit (character page)
  (let ((character-function (compile nil (create-program character)))
        (max-value 0)
        (max-row 0)
        (max-column 0))
    (loop for row from 0 below 2000
          for value = (initial-value character page row 0)
          do (when (> value max-value)
               (setf max-value value
                     max-row row
                     max-column 0))
             (loop for column from 0 below 900
                   do (incf value (funcall character-function page row column))
                   do (when (> value max-value)
                        (setf max-value value
                              max-row row
                              max-column column))))
    (values max-value max-row max-column)))

(defun doit2 (character page)
  (let ((character-function (compile nil (create-program character)))
        (result (make-array 10000 :adjustable t :fill-pointer 0)))
    (loop for row from 0 below 2000
          for value = (initial-value character page row 0)
          do (when (> value 450000)
               (vector-push-extend (list value row 0) result))
             (loop for column from 0 below 900
                   do (incf value (funcall character-function page row column))
                      (when (> value 450000)
                        (vector-push-extend (list value row column) result))))
    result))
