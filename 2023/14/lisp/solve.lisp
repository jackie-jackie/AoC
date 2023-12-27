(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))


(defmacro tilt (grid direction)
  (let* ((vertical-p (ecase direction (:north t) (:south t) (:west nil) (:east nil)))
         (to-0-p (ecase direction (:west t) (:north t) (:east nil) (:south nil)))
         (dimension-x `(array-dimension ,grid ,(if vertical-p 1 0)))
         (dimension-y `(array-dimension ,grid ,(if vertical-p 0 1)))
         (x (if vertical-p 'col 'row))
         (y (if vertical-p 'row 'col)))
    `(loop for ,x from 0 below ,dimension-x
           do (loop with bottom = ,(if to-0-p 0 `(1- ,dimension-y))
                    for ,y from bottom ,@(if to-0-p `(below ,dimension-y) `(downto 0))
                    for c = (aref grid row col)
                    if (char= c #\#) do (setf bottom (,(if to-0-p '1+ '1-) ,y))
                    else if (char= c #\O)
                    do (setf (aref grid row col) #\.
                             (aref grid ,@(if vertical-p `(bottom col) `(row bottom))) #\O)
                    and do (,(if to-0-p 'incf 'decf) bottom)))))

(defun cycle (grid)
  (tilt grid :north)
  (tilt grid :west)
  (tilt grid :south)
  (tilt grid :east))

(defun total-load (grid)
  (loop for row from 0 below (array-dimension grid 0)
        for row-load from (array-dimension grid 0) downto 0
        sum (loop for col from 0 below (array-dimension grid 1)
                  if (char= #\O (aref grid row col))
                  sum row-load)))

(defun grid-id (grid)
  (loop with flat-grid = (flat-grid grid)
        for index from 0 below (array-total-size flat-grid)
        if (char= (aref flat-grid index) #\O) collect index))

(let ((grid (make-grid (parse-input)))
      (cycles 1000000000))
  (tilt grid :north)
  (format t "~D~&" (total-load grid))
  (multiple-value-bind (offset period)
    (loop repeat cycles
          for nil = (cycle grid)
          for id = (grid-id grid)
          for loop = (position id history :test #'equal)
          if loop return (values (1+ loop) (nthcdr loop load-history))
          collect id into history
          collect (total-load grid) into load-history)
    (format t "~D~&" (nth (mod (- cycles offset) (length period)) period))))
