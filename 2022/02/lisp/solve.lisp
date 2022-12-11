(load "../../../common/lisp/util.lisp")

(defun score (opp self)
  (1+ (mod (+ (* opp 6) (* self 4) 3) 9))
  )

(let ((rps (parse-input :pre (lambda (line)
                               (cons (char- (aref line 0) #\A)
                                     (char- (aref line 2) #\X))))))
  (format t "~D~&" (loop for (opp . self) in rps
                         sum (score opp self)))
  (format t "~D~&" (loop for (opp . result) in rps
                         sum (score opp (mod (+ opp result 2) 3))))
  )
