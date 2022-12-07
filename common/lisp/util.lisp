(defun parse-input (&key (pre #'identity) until)
  "Collect lines from stdin and apply optional function to every line."
  (loop for line = (read-line *standard-input* nil :eof)
        until (or (if until (string-equal line until) nil) (eq line :eof))
        collect (funcall pre line))
  ) 

(defun split-sequence (delim seq)
  "Split a sequence into subsequences seperated by delimiter."
  (let ((index (position delim seq)))
    (if index
        (cons (subseq seq 0 index) (split-sequence delim (subseq seq (1+ index))))
        (cons seq nil))
    )
  )

(defun char- (a b)
  "Get the difference in character code points."
  (- (char-code a) (char-code b))
  )

(defun flatten (l)
  "Flatten a list."
  (loop for x in l
        if (listp x)
          append (flatten x)
        else
          collect x)
  )
