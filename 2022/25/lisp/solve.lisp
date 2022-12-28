(load "../../../common/lisp/util.lisp")

(defun snafu-int (snafu)
  (loop with acc = 0
        for c across snafu
        do (setf acc (+ (* 5 acc) (case c (#\= -2) (#\- -1) (t (digit-char-p c)))))
        finally (return acc))
  )

(defun int-snafu (int)
  (coerce (reverse (loop for i = int then (+ (floor i 5) (if (> (mod i 5) 2) 1 0))
                         until (zerop i)
                         collect (case (mod i 5)
                                   (4 #\-) (3 #\=) (t (digit-char (mod i 5))))))
          'string)
  )

(let ((numbers (parse-input)))
  (format t "~A~&" (int-snafu (reduce #'+ (mapcar #'snafu-int numbers))))
  )
