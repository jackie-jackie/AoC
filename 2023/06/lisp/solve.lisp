(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun solve-race (race)
  "Count solutions for x in N for equation x * (time - x) > distance
   where (time . distance) = race."
  (destructuring-bind (time . distance) race
    (if (< distance (* time time 1/4))
        (let ((sqrt (sqrt (- (* time time) (* 4 distance)))))
          (- (ceiling (+ time sqrt) 2) (floor (- time sqrt) 2) 1))
        0)))

(let* ((input (parse-input))
       (races (apply #'pairlis (mapcar (lambda (line)
                                         (mapcar #'parse-integer
                                                 (rest (split-space line))))
                                       input)))
       (race (apply #'pairlis (mapcar (lambda (line)
                                        (mapcar #'parse-integer
                                                (rest (split-sequence (remove #\  line)
                                                                      #\:))))
                                      input))))
  (format t "~D~&" (reduce #'* (mapcar #'solve-race races)))
  (format t "~D~&" (solve-race (first race))))
