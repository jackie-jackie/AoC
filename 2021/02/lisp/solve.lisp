(load "../../../common/lisp/util.lisp")

(let ((input (parse-input :pre (lambda (line)
                                 (destructuring-bind (dir cnt) (split-space line)
                                   (cons dir (parse-integer cnt))
                                   )
                                 ))))
  (format t "~D~&" (loop for (dir . cnt) in input
                         if (string= dir "forward")
                           sum cnt into forward
                         else if (string= dir "down")
                           sum cnt into depth
                         else if (string= dir "up")
                           sum (- cnt) into depth
                         finally
                           (return (* forward depth))
                         ))
  (format t "~D~&" (loop for (dir . cnt) in input
                         if (string= dir "forward")
                           sum cnt into forward
                         and
                           sum (* cnt aim) into depth
                         else if (string= dir "down")
                           sum cnt into aim
                         else if (string= dir "up")
                           sum (- cnt) into aim
                         finally
                           (return (* forward depth))
                         ))
  )
