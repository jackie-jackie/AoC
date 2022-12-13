(load "../../../common/lisp/util.lisp")

(defun parse-line (line)
  (loop for c across line
        for i from 0
        do (setf (aref line i) (case c (#\[ #\() (#\] #\)) (#\, #\ ) (t c))))
  (read-from-string line)
  )

(defun signal< (a b)
  (cond
    ((and (numberp a) (numberp b)) (< a b))
    ((and (listp a) (listp b)) (loop for ai in a
                                     for bi in b
                                     if (signal< ai bi)
                                       return t
                                     else if (signal< bi ai)
                                       return nil
                                     finally
                                       (return (< (length a) (length b)))))
    ((and (numberp a) (listp b)) (signal< (list a) b))
    ((and (listp a) (numberp b)) (signal< a (list b)))
    (t (error "Invalid signal structure."))
    )
  )

(let ((signal-pairs (loop for pair = (parse-input :until "" :pre parse-line)
                          while pair
                          collect pair)))
  (format t "~D~&" (loop for (a b) in signal-pairs
                         for i from 1
                         if (signal< a b)
                           sum i))
  (format t "~D~&" (let ((sorted (sort (apply #'append '(((2)) ((6))) signal-pairs)
                                       #'signal<)))
                     (* (1+ (position '((2)) sorted :test #'equal))
                        (1+ (position '((6)) sorted :test #'equal)))))
  )
