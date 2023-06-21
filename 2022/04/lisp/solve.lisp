(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(let ((pairs (parse-input :pre (lambda (line)
                                 (mapcar #'parse-integer
                                         (split-sequence line #\, #\-))))))
  (format t "~D~&" (loop for (a b c d) in pairs
                         count (or (<= a c d b) (<= c a b d))))
  (format t "~D~&" (loop for (a b c d) in pairs
                         count (and (>= b c) (>= d a))))
  )
