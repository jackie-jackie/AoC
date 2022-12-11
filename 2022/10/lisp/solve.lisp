(load "../../../common/lisp/util.lisp")

(let ((instr (loop for i in (parse-input :pre (lambda (line)
                                                (let ((split (split-sequence #\  line)))
                                                  (cons (car split)
                                                        (if (cadr split)
                                                            (parse-integer (cadr split))
                                                            0))
                                                  )
                                                ))
                   if (string= (car i) "addx")
                     collect '("noop" . 0)
                   collect i)))
  (format t "~D~&" (loop for pc from 1
                         for i in instr
                         and x = 1 then (+ (cdr i) x)
                         if (zerop (mod (- pc 20) 40))
                           sum (* pc x)
                         ))
  (format t "~A~&" (coerce (loop for pc from 0
                                 for i in instr
                                 and x = 1 then (+ (cdr i) x)
                                 if (<= (abs (- x (mod pc 40))) 1)
                                   collect #\#
                                 else
                                   collect #\.
                                 if (zerop (mod (1+ pc) 40))
                                   collect #\Newline)
                           'string))
  )
