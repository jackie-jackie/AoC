(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun expand (positions map unvisited)
  (loop for (x . y) in positions
        for elev = (1- (aref map x y))
        if (and (aref unvisited x y)
                (ignore-errors (aref unvisited (1+ x) y))
                (>= (aref map (1+ x) y) elev))
          collect (cons (1+ x) y)
        if (and (aref unvisited x y)
                (ignore-errors (aref unvisited (1- x) y))
                (>= (aref map (1- x) y) elev))
          collect (cons (1- x) y)
        if (and (aref unvisited x y)
                (ignore-errors (aref unvisited x (1+ y)))
                (>= (aref map x (1+ y)) elev))
          collect (cons x (1+ y))
        if (and (aref unvisited x y)
                (ignore-errors (aref unvisited x (1- y)))
                (>= (aref map x (1- y)) elev))
          collect (cons x (1- y))
        do (setf (aref unvisited x y) nil)))

(defun find-path (start target map)
  (let ((unvisited (make-array (array-dimensions map) :initial-element t)))
    (loop for positions = (list start) then (expand positions map unvisited)
          for steps from 0
          until (find-if target positions)
          finally (return steps))))

(let* ((map (make-grid (parse-input :pre (lambda (line)
                                           (map 'list #'char-code line)))))
       (start (position-grid (char-code #\S) map))
       (end (position-grid (char-code #\E) map)))
  (setf (aref map (car start) (cdr start)) (char-code #\a)
        (aref map (car end) (cdr end)) (char-code #\z))

  (format t "~D~&" (find-path end (curry #'equal start) map))
  (format t "~D~&" (find-path end
                              (lambda (x)
                                (= (aref map (car x) (cdr x)) (char-code #\a)))
                              map)))
