(load "../../../common/lisp/util.lisp")

(defun pow2 (power)
  (if (zerop power) 1 (* 2 (pow2 (1- power))))
  )

(let ((input (cons 0 (sort (parse-input :pre parse-integer) #'<))))
  (format t "~D~&" (loop for (a b) on input
                         for diff = (- (if b b (+ a 3)) a)
                         count (= diff 1) into ones
                         count (= diff 3) into threes
                         finally (return (* ones threes))))
  (format t "~D~&" (reduce #'* (loop for (a b) on input
                                     for diff = (- (if b b (+ a 3)) a)
                                     and l = 1 then (if (= diff 1) (1+ l) 1)
                                     if (= diff 3)
                                       collect (- (pow2 (max 0 (- l 2)))
                                                  (pow2 (max 0 (- l 4)))
                                                  -1))))
  )
