(load "../../../common/lisp/util.lisp")

(defmacro stack-move (from to &optional count)
  (if count
      `(let* ((head ,from)
              (tail (nthcdr (1- ,count) ,from)))
         (setf ,from (cdr tail))
         (setf (cdr tail) ,to)
         (setf ,to head)
         )
      `(push (pop ,from) ,to)
      )
  )

(let ((stacks (apply #'vector
                     (apply #'mapcar
                            (lambda (&rest chars)
                              (remove #\  chars)
                              )
                            (parse-input :until ""
                                         :pre (lambda (line)
                                                (loop for i = 1 then (+ i 4)
                                                      while (< i (length line))
                                                      collect (char line i))
                                                )
                                         ))))
      (instr (parse-input :pre (lambda (line)
                                 (loop for x in (cdr (split-sequence line #\ )) by #'cddr
                                       for offset = 0 then -1
                                       collect (+ offset (parse-integer x)))
                                 )
                          )))
  (let ((stacks (copy-seq stacks)))
    (loop for (cnt from to) in instr
          do (loop repeat cnt
                   do (stack-move (aref stacks from) (aref stacks to))))
    (format t "~A~&" (map 'string #'car stacks))
    )

  (loop for (cnt from to) in instr
        do (stack-move (aref stacks from) (aref stacks to) cnt))
  (format t "~A~&" (map 'string #'car stacks))
  )
