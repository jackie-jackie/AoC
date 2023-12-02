(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(let* ((input (parse-input :pre #'parse-integer))
       (invalid (loop for head on input
                      for num in (nthcdr 25 input)
                      if (not (loop for i in head
                                    if (find (- num i) head) return t))
                        return num)))
  (format t "~D~&" invalid)
  (format t "~D~&" (loop with start = input
                         for end on input
                         sum (car end) into acc
                         do (loop until (<= acc invalid)
                                  do (decf acc (car start))
                                     (setf start (cdr start)))
                         if (= acc invalid)
                           return (loop for n on start
                                        until (eql n end)
                                        minimize (car n) into min
                                        maximize (car n) into max
                                        finally (return (+ min max)))))
  )
