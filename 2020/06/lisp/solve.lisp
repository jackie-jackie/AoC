(load "../../../common/lisp/util.lisp")

(let ((groups (loop for group = (parse-input :until "")
                    while group
                    collect group)))
  (format t "~D~&" (loop for g in groups
                         sum (count-unique (sort (mapcan (lambda (x)
                                                           (coerce x 'list)
                                                           )
                                                         g)
                                                 #'char<=
                                                 ) 
                                           )))
  (format t "~D~&" (loop for g in groups
                         sum (loop for c across (car g)
                                   count (every (lambda (s) (find c s)) (cdr g)))))
  )
