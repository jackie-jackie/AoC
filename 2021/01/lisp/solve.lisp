(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(let ((input (parse-input :pre #'parse-integer)))
  (format t "~D~&" (loop for (a b) on input
                         while b
                         count (< a b)))
  (format t "~D~&" (loop for (a b c d) on input
                         while d
                         count (< (+ a b c) (+ b c d))))
  )
