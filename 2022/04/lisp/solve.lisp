(load "../../../common/lisp/util.lisp")

(let ((pairs (parse-input (lambda (line)
                            (mapcar #'parse-integer
                                    (mapcan (lambda (s)
                                              (split-sequence #\- s))
                                            (split-sequence #\, line)))))))
  (format t "~D~%" (loop for (a b c d) in pairs
                         count (or (<= a c d b) (<= c a b d))))
  (format t "~D~%" (loop for (a b c d) in pairs
                         count (and (>= b c) (>= d a))))
  )
