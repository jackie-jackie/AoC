(load "../../../common/lisp/util.lisp")

(defun move (pos dir)
  (case dir
    (#\L (cons (1- (car pos)) (cdr pos)))
    (#\R (cons (1+ (car pos)) (cdr pos)))
    (#\U (cons (car pos) (1+ (cdr pos))))
    (#\D (cons (car pos) (1- (cdr pos))))
    )
  )

(defun follow (from to)
  (let ((dx (- (car to) (car from)))
        (dy (- (cdr to) (cdr from))))
    (if (> (max (abs dx) (abs dy)) 1)
        (cons (if (> (abs dx) 0) (+ (car from) (signum dx)) (car from))
              (if (> (abs dy) 0) (+ (cdr from) (signum dy)) (cdr from)))
        from)
    )
  )

(defun count-unique-pair (l)
  (count-unique (sort l (lambda (a b)
                          (if (= (car a) (car b))
                              (<= (cdr a) (cdr b))
                              (<= (car a) (car b)))
                          )))
  )

(defun simulate (instr knots)
  (loop for dir in (loop for i in instr
                         append (loop repeat (cdr i)
                                      collect (car i)))
        for head = (move '(0 . 0) dir) then (move head dir)
        for tail = (loop repeat knots collect '(0 . 0))
            then (loop for b in tail
                       for a = (follow b head) then (follow b a)
                       collect a)
        collect (car (last tail)))
  )

(let ((instr (parse-input :pre (lambda (line)
                                 (let ((split (split-sequence #\  line)))
                                   (cons (aref (car split) 0)
                                         (parse-integer (cadr split)))
                                   )
                                 ))))
  (format t "~D~&" (count-unique-pair (simulate instr 1)))
  (format t "~D~&" (count-unique-pair (simulate instr 9)))
  )
