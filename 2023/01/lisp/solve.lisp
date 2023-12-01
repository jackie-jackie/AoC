(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun calibration-value (line)
  (+ (* 10
        (digit-char-p (find-if #'digit-char-p line)))
     (digit-char-p (find-if #'digit-char-p line :from-end t))))

(defun english-to-int (line)
  (loop with line = (copy-seq line)
        for n from 1 to 9
        for english = (format nil "~R" n)
        for first = (search english line)
        for last = (search english line :from-end t)
        if first do (setf (char line (1+ first)) (digit-char n))
        if last do (setf (char line (1+ last)) (digit-char n))
        finally (return line)))

(let ((input (parse-input)))
  (format t "~D~&" (reduce #'+ (mapcar #'calibration-value input)))
  (format t "~D~&" (reduce #'+ (mapcar #'calibration-value
                                       (mapcar #'english-to-int input)))))
