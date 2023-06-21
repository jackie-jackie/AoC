(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun move (pos dir)
  (destructuring-bind (x . y) pos
    (case dir
      (#\L (cons (1- x) y))
      (#\R (cons (1+ x) y))
      (#\U (cons x (1+ y)))
      (#\D (cons x (1- y)))
      )
    )
  )

(defun follow (from to)
  (destructuring-bind (from-x . from-y) from
    (destructuring-bind (to-x . to-y) to
      (let ((dx (- to-x from-x))
            (dy (- to-y from-y)))
        (if (> (max (abs dx) (abs dy)) 1)
            (cons (if (> (abs dx) 0) (+ from-x (signum dx)) from-x)
                  (if (> (abs dy) 0) (+ from-y (signum dy)) from-y))
            from)
        )
      )
    )
  )

(defun count-unique-pair (l)
  (length (remove-duplicates (stable-sort (sort l #'< :key #'car) #'< :key #'cdr)
                             :test #'equal))
  )

(defun simulate (instr knots)
  (loop for dir in (loop for (d . cnt) in instr
                         append (loop repeat cnt
                                      collect d))
        for head = (move '(0 . 0) dir) then (move head dir)
        for tail = (loop repeat knots collect '(0 . 0))
            then (loop for b in tail
                       for a = (follow b head) then (follow b a)
                       collect a)
        collect (car (last tail)))
  )

(let ((instr (parse-input :pre (lambda (line)
                                 (destructuring-bind (dir cnt) (split-space line)
                                   (cons (char dir 0) (parse-integer cnt))
                                   )
                                 ))))
  (format t "~D~&" (count-unique-pair (simulate instr 1)))
  (format t "~D~&" (count-unique-pair (simulate instr 9)))
  )
