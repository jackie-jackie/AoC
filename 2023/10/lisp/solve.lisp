(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun pipe-connections (grid row col)
  "List of coordinates of connected pipes."
  (let ((pipe (aref grid row col)))
    (cond ((char= pipe #\-) (list (cons row (1+ col)) (cons row (1- col))))
          ((char= pipe #\|) (list (cons (1+ row) col) (cons (1- row) col)))
          ((char= pipe #\7) (list (cons row (1- col)) (cons (1+ row) col)))
          ((char= pipe #\J) (list (cons row (1- col)) (cons (1- row) col)))
          ((char= pipe #\L) (list (cons row (1+ col)) (cons (1- row) col)))
          ((char= pipe #\F) (list (cons row (1+ col)) (cons (1+ row) col)))
          (t '()))))

(defun start-connections (grid start-row start-y)
  "List of coordinates of connected pipes.
   Like #'pipe-connections but does not rely on the pipe's symbol in the grid."
  (loop for row from (1- start-row) to (1+ start-row)
        append (loop for col from (1- start-y) to (1+ start-y)
                     if (ignore-errors (find (cons start-row start-y)
                                             (pipe-connections grid row col)
                                             :test #'equal))
                     collect (cons row col))))

(let* ((grid (make-grid (parse-input)))
       (start (position-grid #\S grid))
       (loop (loop for coords = (first (start-connections grid (car start) (cdr start)))
                   then (find-if-not (curry #'equal old-coords)
                                     (pipe-connections grid (car coords) (cdr coords)))
                   and old-coords = start then coords
                   while coords collect coords)))
  ;; fix the start symbol
  (loop with connections = (start-connections grid (car start) (cdr start))
        for pipe in '(#\| #\- #\7 #\L #\J #\F)
        do (setf (aref grid (car start) (cdr start)) pipe)
        until (= 2 (length (intersection connections
                                         (pipe-connections grid (car start) (cdr start))
                                         :test #'equal))))
  ;; clean up the grid by removing all non-loop pipes
  (loop with new-grid = (make-array (array-dimensions grid) :initial-element #\.)
        for (row . col) in loop
        do (setf (aref new-grid row col) (aref grid row col))
        finally (setf grid new-grid))
  (format t "~D~&" (floor (length loop) 2))
  (format t "~D~&" (loop with flat-grid = (flat-grid grid)
                         for index from 0 below (array-total-size flat-grid)
                         for pipe = (aref flat-grid index)
                         for boundary = (if (find pipe '(#\F #\L)) pipe boundary)
                         for in-loop = (if (find (cons pipe boundary)
                                                 (list (cons #\| boundary)
                                                       '(#\7 . #\L)
                                                       '(#\J . #\F))
                                                 :test #'equal)
                                           (not in-loop)
                                           in-loop)
                         count (and in-loop (char= pipe #\.)))))
