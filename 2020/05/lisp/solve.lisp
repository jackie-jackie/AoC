(load "../../../common/lisp/util.lisp")

(let ((seats (parse-input :pre (lambda (line)
                                 (loop for c across line
                                       with rowl = 0
                                       with rowh = 128
                                       with coll = 0
                                       with colh = 8
                                       do (case c
                                            (#\F (decf rowh (/ (- rowh rowl) 2)))
                                            (#\B (incf rowl (/ (- rowh rowl) 2)))
                                            (#\L (decf colh (/ (- colh coll) 2)))
                                            (#\R (incf coll (/ (- colh coll) 2)))
                                            )
                                       finally (return (+ (* rowl 8) coll)))
                                 ))))
  (format t "~D~&" (reduce #'max seats))
  (format t "~D~&" (loop for (a b) on (sort seats #'<=)
                         while b
                         if (= (- b a) 2) return (1+ a)))
  )
