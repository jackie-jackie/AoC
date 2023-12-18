(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun parse-line (line)
  (destructuring-bind (direction count color) (split-space line)
    (list (read-from-string (concatenate 'string ":" direction))
          (parse-integer count)
          (ecase (char color 7) (#\0 :R) (#\1 :D) (#\2 :L) (#\3 :U))
          (parse-integer color :start 2 :end 7 :radix 16))))

(defun dig (dig-plan)
  (let* ((path (loop for (direction count) in dig-plan
                     and prev-direction = (caar (last dig-plan)) then direction
                     and r = 0
                     then (ecase direction (:U (- r count)) (:D (+ r count)) ((:L :R) r))
                     and c = 0
                     then (ecase direction (:L (- c count)) (:R (+ c count)) ((:U :D) c))
                     collect (list r c
                                   (ecase direction
                                     (:U (ecase prev-direction (:L #\L) (:R #\J)))
                                     (:D (ecase prev-direction (:L #\F) (:R #\7)))
                                     (:L (ecase prev-direction (:U #\7) (:D #\J)))
                                     (:R (ecase prev-direction (:U #\F) (:D #\L))))
                                   prev-direction
                                   direction
                                   count)))
         (min-row (reduce #'min path :key #'first))
         (max-row (reduce #'max path :key #'first))
         (min-col (reduce #'min path :key #'second))
         (max-col (reduce #'max path :key #'second))
         (grid (make-array (list (1+ (- max-row min-row)) (1+ (- max-col min-col)))
                           :initial-element #\.)))
    ;; modify the problem to fit into the solution of 2023-12-10
    (loop for p in path
          do (decf (first p) min-row)
          do (decf (second p) min-col))
    (loop for (row col symbol nil direction count) in path
          do (setf (aref grid row col) symbol)
          do (ecase direction
               (:U (loop for r downfrom (1- row)
                         repeat (1- count)
                         do (setf (aref grid r col) #\|)))
               (:D (loop for r from (1+ row)
                         repeat (1- count)
                         do (setf (aref grid r col) #\|)))
               (:L (loop for c downfrom (1- col)
                         repeat (1- count)
                         do (setf (aref grid row c) #\-)))
               (:R (loop for c from (1+ col)
                         repeat (1- count)
                         do (setf (aref grid row c) #\-)))))
    ;; solution copied from 2023-12-10
    (loop with flat-grid = (flat-grid grid)
          for index from 0 below (array-total-size flat-grid)
          for pipe = (aref flat-grid index)
          for boundary = (if (find pipe '(#\F #\L)) pipe boundary)
          for in-loop = (if (find (cons pipe boundary)
                                  (list (cons #\| boundary) '(#\7 . #\L) '(#\J . #\F))
                                  :test #'equal)
                            (not in-loop)
                            in-loop)
          count (or in-loop (char/= pipe #\.)))))

(let* ((dig-plan (parse-input :pre #'parse-line)))
  (format t "~D~&" (dig dig-plan))
  (quit) ; TODO too big :(
  (format t "~D~&" (dig (mapcar #'cddr dig-plan))))
