(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun extrapolate (backwards history)
  (loop for h = history then (mapcar #'- (rest h) h)
        until (every #'zerop h)
        collect (first (if backwards h (last h))) into ex
        finally (return (if backwards
                            (reduce (lambda (a b) (- a b)) ex :from-end t)
                            (reduce #'+ ex)))))

(let ((input (parse-input :pre (lambda (line)
                                 (mapcar #'parse-integer (split-space line))))))
  (format t "~D~&" (reduce #'+ (mapcar (curry #'extrapolate nil) input)))
  (format t "~D~&" (reduce #'+ (mapcar (curry #'extrapolate t) input))))
