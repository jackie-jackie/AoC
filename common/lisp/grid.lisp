(defun make-grid (initial-contents)
  "Create a 2 dimensional array from sequence"
  (make-array (list (length initial-contents) (length (car initial-contents)))
              :initial-contents initial-contents)
  )

(defun position-grid (item grid)
  "Pair of indices of item in grid."
  (loop for x from 0 below (array-dimension grid 0)
        for y = (loop for y from 0 below (array-dimension grid 1)
                      if (equal item (aref grid x y)) return y)
        if y return (cons x y))
  )

(defun grid-stencil (selector kernel grid &key target-grid)
  (loop for x from 0 below (array-dimension grid 0)
        sum (loop for y from 0 below (array-dimension grid 1)
                  count (not (equal (aref grid x y)
                                    (setf (aref (if target-grid target-grid grid) x y)
                                          (apply kernel
                                                 (aref grid x y)
                                                 (funcall selector grid x y)))))))
  )

(defun grid-stencil9 (kernel grid &key target-grid)
  (grid-stencil (lambda (grid x y)
                  (loop for nx from (1- x) to (1+ x)
                        append (loop for ny from (1- y) to (1+ y)
                                     if (and (array-in-bounds-p grid nx ny)
                                             (or (/= x nx) (/= y ny)))
                                     collect (aref grid nx ny)))
                  )
                kernel
                grid
                :target-grid target-grid)
  )
