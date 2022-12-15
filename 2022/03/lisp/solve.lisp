(load "../../../common/lisp/util.lisp")

(defun value (c)
  (+ 1 (char- (char-downcase c) #\a) (if (char<= c #\Z) 26 0))
  )

(let ((sacks (parse-input :pre (lambda (line) (coerce line 'list)))))
  (format t "~D~&" (loop for sack in sacks
                         sum (value (car (intersection (subseq sack (/ (length sack) 2))
                                                       (subseq sack 0 (/ (length sack) 2)))))))
  (format t "~D~&" (loop for (s0 s1 s2) on sacks by #'cdddr
                         sum (value (car (reduce #'intersection (list s0 s1 s2))))))
  )
