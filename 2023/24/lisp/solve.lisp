(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun intersects (line1 line2)
  (let* ((m1 (/ (fifth line1) (fourth line1)))
         (t1 (- (second line1) (* m1 (first line1))))
         (m2 (/ (fifth line2) (fourth line2)))
         (t2 (- (second line2) (* m2 (first line2))))
         (x (ignore-errors (/ (- t2 t1) (- m1 m2)))))
    (ignore-errors (list (/ (- x (first line1)) (fourth line1))
                         (/ (- x (first line2)) (fourth line2))
                         x
                         (+ (* m1 x) t1)))))

(let* ((input (parse-input :pre (lambda (line)
                                  (mapcar #'parse-integer
                                          (split-sequence line #\  #\, #\@)))))
       (min 200000000000000)
       (max 400000000000000))
  (format t "~D~&" (loop for (line1 . rest) on input
                         sum (loop for line2 in rest
                                   for (t1 t2 x y) = (intersects line1 line2)
                                   count (ignore-errors (and (<= min x max)
                                                             (<= min y max)
                                                             (> t1 0)
                                                             (> t2 0))))))
  (format t "~D~&" nil))
