(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun update (required self neighbors)
  (let ((occ (count #\# neighbors)))
    (cond
      ((and (char= self #\L) (zerop occ)) #\#)
      ((and (char= self #\#) (>= occ required)) #\L)
      (t self)
      )
    )
  )

(defun simulate1 (grid)
  (loop for new = (make-array (array-dimensions grid))
        and old = grid then new
        until (zerop (grid-stencil9 (lambda (self &rest neighbors)
                                      (update 4 self neighbors)
                                      )
                                    old
                                    :target-grid new))
        finally (return new))
  )

(defun in-sight (grid x y)
  (loop for (dx . dy) in '((0 . 1) (0 . -1) (1 . 0) (-1 . 0)
                           (1 . 1) (1 . -1) (-1 . 1) (-1 . -1))
        for n = (loop for nx = (+ x dx) then (+ nx dx)
                      for ny = (+ y dy) then (+ ny dy)
                      while (ignore-errors (char= (aref grid nx ny) #\.))
                      finally
                        (return (ignore-errors (aref grid nx ny))))
        if n collect n)
  )

(defun simulate2 (grid)
  (loop for new = (make-array (array-dimensions grid))
        and old = grid then new
        until (zerop (grid-stencil #'in-sight
                                   (lambda (self &rest neighbors)
                                     (update 5 self neighbors)
                                     )
                                   old
                                   :target-grid new))
        finally (return new))
  )

(defun count-occupied (grid)
  (loop for x from 0 below (array-dimension grid 0)
        sum (loop for y from 0 below (array-dimension grid 1)
                  count (char= (aref grid x y) #\#)))
  )

(let ((grid (make-grid (parse-input))))
  (format t "~D~&" (count-occupied (simulate1 grid)))
  (format t "~D~&" (count-occupied (simulate2 grid)))
  )
