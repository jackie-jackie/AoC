(defun parse-input (&key pre until)
  "Collect lines from stdin and apply optional function to every line."
  (loop for line = (read-line *standard-input* nil :eof)
        until (or (string-equal line until) (eql line :eof))
        collect (if pre (funcall pre line) line)))

(defun split-sequence (seq &rest delims)
  "Split a sequence into subsequences seperated by delimiter."
  (loop for start = 0 then (1+ end)
        for end = (position-if (lambda (item) (find item delims)) seq :start start)
        for subs = (subseq seq start end)
        unless (zerop (length subs)) collect subs into split
        unless end return split))

(defun split-space (seq)
  (split-sequence seq #\ ))

(defun char- (a b)
  "Get the difference in character code points."
  (- (char-code a) (char-code b)))

(defun flatten (l)
  "Flatten a list."
  (loop for x in l
        if (listp x)
          append (flatten x)
        else
          collect x))

(defmacro curry (function &rest fixed-args)
  `(lambda (&rest args)
     ,(append `(apply ,function) fixed-args `(args))))
