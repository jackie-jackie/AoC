(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun longest-hike (grid start end &optional previous)
  (if (equal start end)
      (length previous)
      (loop for delta in '((-1 . 0) (0 . -1) (0 . 1) (1 . 0))
            for x = (+ (car start) (car delta))
            for y = (+ (cdr start) (cdr delta))
            for char = (if (array-in-bounds-p grid x y) (aref grid x y) #\#)
            if (and (not (find (cons x y) previous :test #'equal))
                    (ecase char
                      (#\. t)
                      (#\^ (equal delta '(-1 . 0)))
                      (#\> (equal delta '(0 . 1)))
                      (#\v (equal delta '(1 . 0)))
                      (#\< (equal delta '(0 . -1)))
                      (#\# nil)))
            maximize (longest-hike grid (cons x y) end (cons start previous)))))

(let* ((grid (make-grid (parse-input)))
       (start (position-grid #\. grid))
       (end (cons (1- (array-dimension grid 1))
                  (position #\.
                            (make-array
                              (array-dimension grid 1)
                              :displaced-to grid
                              :displaced-index-offset (- (array-total-size grid)
                                                         (array-dimension grid 1 )))))))
  (format t "~D~&" (longest-hike grid start end))
  (grid-stencil1 (lambda (self) (if (char= self #\#) #\# #\.)) grid)
  (quit) ; too slow
  (format t "~D~&" (longest-hike grid start end)))
