(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defmacro reflect (beam mirror)
  (let ((op (case mirror (#\\ '+) (#\/ '-))))
    `(cons (cons (,op (caar ,beam) (cddr ,beam)) (,op (cdar ,beam) (cadr ,beam)))
           (cons (,op (cddr ,beam)) (,op (cadr ,beam))))))

(defun direction-mask (drow dcol)
  (ecase dcol (-1 1) (1 2) (0 (ecase drow (-1 4) (1 8)))))

(defun trace-beam (beam grid &optional (acc (make-array (array-dimensions grid)
                                                        :initial-element 0)))
  "Traces a beam through the grid of mirrors and splitters.
   Returns an array of dimensions equal to grid containing 0 if a field has not
   been passed through by the beam or a non-zero integer otherwise."
  (destructuring-bind ((row . col) . (drow . dcol)) beam
    (cond
      ((not (array-in-bounds-p grid row col)) acc)
      ((not (zerop (logand (aref acc row col) (direction-mask drow dcol)))) acc)
      ((case (aref grid row col) (#\. t) (#\| (zerop dcol)) (#\- (zerop drow)))
       (loop for r = row then (+ r drow)
             for c = col then (+ c dcol)
             unless (array-in-bounds-p grid r c) return acc
             unless (zerop (logand (aref acc r c) (direction-mask drow dcol))) return acc
             if (case (aref grid r c) (#\. t) (#\| (zerop dcol)) (#\- (zerop drow)))
             do (incf (aref acc r c) (direction-mask drow dcol))
             else return (trace-beam (acons r c (cdr beam)) grid acc)))
      (t (incf (aref acc row col) (direction-mask drow dcol))
         (case (aref grid row col)
           (#\/ (trace-beam (reflect beam #\/) grid acc))
           (#\\ (trace-beam (reflect beam #\\) grid acc))
           (otherwise (trace-beam (reflect beam #\/)
                                  grid
                                  (trace-beam (reflect beam #\\) grid acc))))))))

(defun energized (beam grid)
  "Count fields in grid that have been passed through by the beam."
  (loop with flat-grid = (flat-grid (trace-beam beam grid))
        for index from 0 below (array-total-size grid)
        count (/= 0 (aref flat-grid index))))

(let ((grid (make-grid (parse-input))))
  (format t "~D~&" (energized '((0 . 0) . (0 . 1)) grid))
  (format t "~D~&" (max (loop for i from 0 below (array-dimension grid 0)
                              maximize (energized `((,i . 0) . (0 . 1)) grid)
                              maximize (energized `((,i . ,(1- (array-dimension grid 1)))
                                                    . (0 . -1))
                                                  grid))
                        (loop for i from 0 below (array-dimension grid 1)
                              maximize (energized `((0 . ,i) . (1 . 0)) grid)
                              maximize (energized `((,(1- (array-dimension grid 0)) . ,i)
                                                    . (-1 . 0))
                                                  grid)))))
