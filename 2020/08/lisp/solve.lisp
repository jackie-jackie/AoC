(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun simulate (instr)
  (loop for pc from 0
        until (>= pc (length instr))
        for (op . val) = (aref instr pc)
        with acc = 0
        do (setf (aref instr pc) '("err" . 0))
        if (string= op "acc")
          do (incf acc val)
        if (string= op "jmp")
          do (incf pc (1- val))
        if (string= op "err")
          return (values acc nil)
        finally (return (values acc t))))

(defun try-fix (instr pos repl)
  (multiple-value-bind
    (acc success)
    (simulate (let ((instr (copy-seq instr)))
                (setf (aref instr pos) repl)
                instr))
    (if success acc nil)))

(let ((instr (coerce (parse-input :pre (lambda (line)
                                         (destructuring-bind
                                           (op val)
                                           (split-space line)
                                           (cons op (parse-integer val)))))
                     'vector)))
  (format t "~D~&" (simulate (copy-seq instr)))
  (format t "~D~&" (loop for (op . val) across instr
                         for i from 0
                         for result = (cond
                                        ((string= op "jmp")
                                         (try-fix instr i (cons "nop" val)))
                                        ((and (string= op "nop") (/= val 0))
                                         (try-fix instr i (cons "jmp" val)))
                                        (t nil))
                         if result return result)))
