(defun score (opp self)
  (1+ (mod (+ (* opp 6) (* self 4) 3) 9)) 
  )

(defun char- (a b)
  (- (char-code a) (char-code b))
  )

(let ((rps (loop for line = (read-line *standard-input* nil :eof)
                 until (eq line :eof)
                 collect (cons (char- (aref line 0) #\A)
                               (char- (aref line 2) #\X)))))
  (format t "~D~%" (loop for (opp . self) in rps
                         sum (score opp self)))
  (format t "~D~%" (loop for (opp . result) in rps
                         sum (score opp (mod (+ opp result 2) 3))))
  )
