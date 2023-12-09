(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun solve (list count sum)
  "Returns the product of count elements in sorted list that sum up to sum"
  (if (= count 1)
      (find sum list)
      (loop for i in list
            for solution = (solve (remove i list) (1- count) (- sum i))
            until (> i sum)
            if solution return (* solution i))))

(let ((input (sort (parse-input :pre #'parse-integer) #'<)))
  (format t "~D~&" (solve input 2 2020))
  (format t "~D~&" (solve input 3 2020)))
