(load "../../../common/lisp/util.lisp")

(defun value (c)
  (+ 1 (char- (char-downcase c) #\a) (if (char<= c #\Z) 26 0))
  )

(defun find-common (array &rest arrays)
  (loop for item across array
        if (every (lambda (a) (find item a)) arrays) 
          return item)
  )

(let ((sacks (parse-input)))
  (format t "~D~%" (loop for sack in sacks
                         sum (value (find-common (subseq sack 0 (/ (length sack) 2))
                                                 (subseq sack (/ (length sack) 2))))))
  (format t "~D~%" (loop for (s0 s1 s2) on sacks by #'cdddr
                         sum (value (find-common s0 s1 s2))))
  )
