(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun queue-insert (new queue priorities)
  (if (or (null queue)
          (< (apply #'aref priorities new) (apply #'aref priorities (first queue))))
      (cons new (delete new queue :test #'equal))
      (cons (first queue) (queue-insert new (rest queue) priorities))))

(defun minimum-heat-loss (grid min-distance max-distance)
  (let ((start `(0 0 0 0))
        (goal-x (1- (array-dimension grid 0)))
        (goal-y (1- (array-dimension grid 1)))
        (g-score (make-array (append `(4 ,(- max-distance min-distance))
                                     (array-dimensions grid))
                             :initial-element most-positive-double-float))
        (f-score (make-array (append `(4 ,(- max-distance min-distance))
                                     (array-dimensions grid))
                             :initial-element most-positive-double-float)))
    (loop for dir from 0 below (array-dimension g-score 0)
          do (loop for cnt from 0 below (array-dimension g-score 1)
                   do (setf (aref g-score dir cnt (third start) (fourth start)) 0)))
    (loop for dir from 0 below (array-dimension g-score 0)
          do (loop for cnt from 0 below (array-dimension g-score 1)
                   do (setf (aref f-score dir cnt (third start) (fourth start))
                            (+ (abs (- (third start) goal-x))
                               (abs (- (third start) goal-y))))))

    (loop with open-set = (list start)
          until (null open-set)
          for (dir cnt x y) = (pop open-set)
          maximize (length open-set) into l
          if (and (= x goal-x) (= y goal-y)) return (aref g-score dir cnt x y)
          do (loop for n-dir from 0 below 4
                   for delta in '(-1 1 -1 1)
                   for true-delta = (if (= n-dir dir) delta (* delta (1+ min-distance)))
                   for neighbor = (list n-dir
                                        (if (= n-dir dir) (1+ cnt) 0)
                                        (if (< n-dir 2) (+ true-delta x) x)
                                        (if (< n-dir 2) y (+ true-delta y)))
                   for (nil n-cnt n-x n-y) = neighbor
                   for g = (when (array-in-bounds-p g-score n-dir n-cnt n-x n-y)
                             (+ (aref g-score dir cnt x y)
                                (loop for d from 1 to (abs true-delta)
                                      sum (aref grid
                                                (if (< n-dir 2) (+ (* d delta) x) x)
                                                (if (< n-dir 2) y (+ (* d delta) y))))))
                   if (and g
                           (/= 1 (mod (+ dir n-dir) 4))
                           (loop for c from 0 to n-cnt
                                 always (< g (aref g-score n-dir c n-x n-y))))
                   do (setf (aref g-score n-dir n-cnt n-x n-y) g)
                   and do (setf (aref f-score n-dir n-cnt n-x n-y)
                                (+ g (+ (abs (- n-x goal-x)) (abs (- n-y goal-y)))))
                   and do (setf open-set (queue-insert neighbor open-set f-score))))))

(let ((grid (make-grid (parse-input :pre (curry #'map 'list #'digit-char-p)))))
  (format t "~D~&" (minimum-heat-loss grid 0 3))
  (format t "~D~&" (minimum-heat-loss grid 3 10)))
