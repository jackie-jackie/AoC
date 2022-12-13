(load "../../../common/lisp/util.lisp")

(defun count-slope (trees right down)
  (loop for row in trees by (lambda (l) (nthcdr down l))
        for offset from 0 by right
        count (char= (aref row (mod offset (length row))) #\#))
  )

(let ((trees (parse-input)))
  (format t "~D~&" (count-slope trees 3 1))
  (format t "~D~&" (reduce #'* (mapcar (lambda (x)
                                         (count-slope trees (car x) (cdr x))
                                         )
                                       '((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2)))))
  )
