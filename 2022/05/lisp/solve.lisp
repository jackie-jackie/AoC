(load "../../../common/lisp/util.lisp")

(defmacro stack-move (from to &optional cnt)
  (if cnt
      `(progn
         (setf ,to (append (subseq ,from 0 ,cnt) ,to))
         (setf ,from (subseq ,from ,cnt))
         )
      `(progn
         (setf ,to (cons (car ,from) ,to))
         (setf ,from (cdr ,from))
         )
      )
  )

(let ((stacks (apply #'vector
                     (apply #'mapcar
                            (lambda (&rest l)
                              (loop for i in l
                                    if (char/= i #\ ) collect i)
                              )
                            (parse-input :until ""
                                         :pre (lambda (line)
                                                (loop for i = 1 then (+ i 4)
                                                      until (>= i (length line))
                                                      collect (aref line i))
                                                )
                                         ))))
      (instr (parse-input :pre (lambda (line)
                                 (loop for x in (cdr (split-sequence #\  line)) by #'cddr
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
