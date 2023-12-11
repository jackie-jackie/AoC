(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun empty-lines (grid &optional columns)
  (loop for x from 0 below (array-dimension grid (if columns 1 0))
        if (loop for y from 0 below (array-dimension grid (if columns 0 1))
                 if (char= (if columns (aref grid y x) (aref grid x y)) #\#) return nil
                 finally (return t))
        collect x))

(defun expand (galaxies empty-rows empty-cols factor)
  (loop for (row . col) in galaxies
        collect (cons (+ row (* (count-if (curry #'> row) empty-rows) (1- factor)))
                      (+ col (* (count-if (curry #'> col) empty-cols) (1- factor))))))

(defun sum-distances (galaxies)
  (loop for ((row-a . col-a) . rest) on galaxies
        sum (loop for (row-b . col-b) in rest
                  sum (+ (abs (- row-b row-a)) (abs (- col-b col-a))))))

(let* ((grid (make-grid (parse-input)))
       (galaxies (loop for row from 0 below (array-dimension grid 0)
                       append (loop for col from 0 below (array-dimension grid 1)
                                    if (char= (aref grid row col) #\#)
                                    collect (cons row col))))
       (empty-rows (empty-lines grid))
       (empty-cols (empty-lines grid t)))
  (format t "~D~&" (sum-distances (expand galaxies empty-rows empty-cols 2)))
  (format t "~D~&" (sum-distances (expand galaxies empty-rows empty-cols 1000000))))
