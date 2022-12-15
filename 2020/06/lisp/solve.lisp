(load "../../../common/lisp/util.lisp")

(let ((groups (loop for group = (parse-input :until ""
                                             :pre (lambda (line)
                                                    (coerce line 'list)
                                                    ))
                    while group
                    collect group)))
  (format t "~D~&" (loop for g in groups
                         sum (length (reduce #'union g))))
  (format t "~D~&" (loop for g in groups
                         sum (length (reduce #'intersection g))))
  )
