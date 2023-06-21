(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(let ((input (parse-input)))
  (format t "~D~&" input)
  (format t "~D~&" nil)
  )
