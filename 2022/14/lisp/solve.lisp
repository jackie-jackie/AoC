(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun parse-line (line)
  (loop for (x y) on (split-sequence line #\  #\- #\> #\,) by #'cddr
        collect (cons (parse-integer x) (parse-integer y)))
  )

(defun drop-sand (grid with-floor)
  (loop for cnt from 0
        while (loop with x = 500
                    for y from 1
                    unless (array-in-bounds-p grid x y)
                      return (and with-floor (setf (aref grid x (1- y)) t))
                    if (and (aref grid x y)
                            (not (ignore-errors (aref grid (1- x) y))))
                      do (decf x)
                    else if (and (aref grid x y)
                                 (not (ignore-errors (aref grid (1+ x) y))))
                      do (incf x)
                    else if (aref grid x y)
                      return (if (and (= x 500) (= y 1))
                                 (progn (incf cnt) nil)
                                 (setf (aref grid x (1- y)) t)))
        finally (return cnt))
  )

(let* ((paths (parse-input :pre #'parse-line))
       (maxheight (reduce #'max (mapcar #'cdr (apply #'append paths))))
       (grid (make-array (list (+ 502 maxheight) (+ 2 maxheight))
                         :initial-element nil)))
  (loop for path in paths
        do (loop for ((sx . sy) (ex . ey)) on path
                 while ex
                 if (= sx ex)
                   do (loop for y from (min sy ey) to (max sy ey)
                            do (setf (aref grid sx y) t))
                 else if (= sy ey)
                   do (loop for x from (min sx ex) to (max sx ex)
                            do (setf (aref grid x sy) t)))
        )
  (let ((solution1 (drop-sand grid nil)))
    (format t "~D~&" solution1)
    (format t "~D~&" (+ solution1 (drop-sand grid t)))
    )
  )
