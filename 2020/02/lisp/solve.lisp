(load "../../../common/lisp/util.lisp")

(defun xor (arg0 arg1)
  (or (and arg0 (not arg1)) (and (not arg0) arg1))
  )

(let ((input (parse-input :pre (lambda (line)
                                 (destructuring-bind
                                   (low high char str)
                                   (split-sequence line #\  #\-)
                                   (list (parse-integer low)
                                         (parse-integer high)
                                         (char char 0)
                                         str)
                                   )
                                 ))))
  (format t "~D~&" (loop for (low high char pwd) in input
                         for cnt = (count char pwd)
                         count (and (>= cnt low) (<= cnt high))))
  (format t "~D~&" (loop for (low high char pwd) in input
                         count (xor (char= (aref pwd (1- low)) char)
                                    (char= (aref pwd (1- high)) char))))
  )
