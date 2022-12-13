(load "../../../common/lisp/util.lisp")

(defun xor (arg0 arg1)
  (or (and arg0 (not arg1)) (and (not arg0) arg1))
  )

(let ((input (parse-input :pre (lambda (line)
                                 (let ((split (split-sequence #\  line)))
                                   (list (mapcar #'parse-integer
                                                 (split-sequence #\- (nth 0 split)))
                                         (aref (nth 1 split) 0)
                                         (nth 2 split))
                                   )
                                 ))))
  (format t "~D~&" (loop for ((low high) char pwd) in input
                         for cnt = (count char pwd)
                         count (and (>= cnt low) (<= cnt high))))
  (format t "~D~&" (loop for ((low high) char pwd) in input
                         count (xor (char= (aref pwd (1- low)) char)
                                    (char= (aref pwd (1- high)) char))))
  )
