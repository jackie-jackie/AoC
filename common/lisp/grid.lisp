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
