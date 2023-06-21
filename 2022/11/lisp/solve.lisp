(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun parse-line (line)
  (destructuring-bind (fst . rst) (split-sequence line #\, #\: #\ )
    (cond
      ((string= fst "Monkey")
       0) ; used as counter for inspected items
      ((string= fst "Starting")
       (loop for i in (cdr rst)
             collect (parse-integer i)))
      ((string= fst "Operation")
       (lambda (old)
         (funcall (case (char (nth 3 rst) 0)
                    (#\+ #'+)
                    (#\* #'*)
                    )
                  (if (string= (nth 2 rst) "old") old (parse-integer (nth 2 rst)))
                  (if (string= (nth 4 rst) "old") old (parse-integer (nth 4 rst))))
         )
       )
      ((string= fst "Test")
       (parse-integer (car (last rst)))
       )
      ((string= fst "If")
       (parse-integer (car (last rst)))
       )
      )
    )
  )

(defun simulate-round (monkeys dec mod)
  (loop for monkey across monkeys
        do (destructuring-bind (items op test succ fail) (cdr monkey)
             (loop for item in items
                   do (incf (car monkey)) ; increase inspection counter
                      (if dec ; adjust worry level
                          (setf item (mod (floor (funcall op item) 3) mod))
                          (setf item (mod (funcall op item) mod)))
                      (push item (cadr (aref monkeys (if (zerop (mod item test))
                                                         succ
                                                         fail))))
                   finally (setf (cadr monkey) nil))
             )
        finally (return monkeys))
  )

(defun simulate (monkeys rounds dec)
  (loop repeat rounds
        do (simulate-round monkeys dec (reduce #'* (map 'list #'cadddr monkeys)))
        finally (return monkeys))
  )

(defun score (monkeys)
  (reduce #'* (subseq (sort (map 'list #'car monkeys) #'>) 0 2))
  )

(let ((monkeys (loop for m = (parse-input :until "" :pre #'parse-line)
                     while m
                     collect m)))
  (format t "~D~&" (score (simulate (coerce (copy-tree monkeys) 'vector) 20 t)))
  (format t "~D~&" (score (simulate (coerce monkeys 'vector) 10000 nil)))
  )
