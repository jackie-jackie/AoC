(load "../../../common/lisp/util.lisp")

(defun parse-dir (data)
  "Convert input to directory tree structure. Does not validate input correctness."
  (loop for d = data then (cdr d)
        until (every #'string= (car d) '("$" "cd" ".."))
        ; recurse into directories
        if (every #'string= (car d) '("$" "cd"))
          collect (multiple-value-bind
                        (dir rest)
                        (parse-dir (cdr d))
                        (setf d rest)
                        dir
                        ) into subnodes
        ; individual files don't matter, just sum total size
        if (and (caar d) (string/= (caar d) "$") (string/= (caar d) "dir"))
          sum (parse-integer (caar d)) into files
        finally (return (values (cons (+ files
                                         (reduce #'+
                                                 (mapcar #'car
                                                         subnodes)))
                                      subnodes)
                                d)))
  )

; first line is always "$ cd /", so we'll skip it
(let ((dirs (flatten (parse-dir (cdr (parse-input :pre (lambda (line)
                                                         (split-sequence #\  line)
                                                         )))))))
  (format t "~D~&" (loop for d in dirs
                         if (<= d 100000)
                           sum d))
  (format t "~D~&" (loop for d in dirs
                         if (<= (- (car dirs) d) 40000000)
                           minimize d))
  )
