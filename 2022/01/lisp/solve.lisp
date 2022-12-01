(let ((elves (loop with end = nil
                   until end
                   collect (loop for line = (read-line *standard-input* nil :eof)
                                 until (or (string-equal line "") (setf end (eq line :eof)))
                                 sum (parse-integer line)))))
  (format t "~D~%" (reduce #'max elves))
  (format t "~D~%" (reduce #'+ (subseq (sort elves #'>) 0 3)))
  )
