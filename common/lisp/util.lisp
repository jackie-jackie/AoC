(defun parse-input (&optional (pre #'identity))
  "Collect lines from stdin and apply optional function to every line."
  (loop for line = (read-line *standard-input* nil :eof)
        until (eq line :eof)
        collect (funcall pre line))
  ) 

(defun split-sequence (delim seq)
  "Split a sequence into subsequences seperated by delimiter."
  (let ((index (position delim seq)))
    (if index
        (list (subseq seq 0 index) (split-sequence delim (subseq seq (1+ index))))
        seq)
    )
  )

(defun char- (a b)
  "Get the difference in character code points."
  (- (char-code a) (char-code b))
  )
