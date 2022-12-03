(defun value (c)
  (+ 1 (- (char-code (char-downcase c)) (char-code #\a)) (if (char< c #\a) 26 0))
  )

(defun find-common (array &rest arrays)
  (loop for item across array
        if (every (lambda (a) (find item a)) arrays) 
          return item)
  )

(let ((sacks (loop for line = (read-line *standard-input* nil :eof)
                   until (eq line :eof)
                   collect line)))
  (format t "~D~%" (loop for sack in sacks
                         sum (value (find-common (subseq sack 0 (/ (length sack) 2))
                                                 (subseq sack (/ (length sack) 2))))))
  (format t "~D~%" (loop for (s0 s1 s2) on sacks by #'cdddr
                         sum (value (find-common s0 s1 s2))))
  )
