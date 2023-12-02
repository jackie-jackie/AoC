(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun maximums (line)
  (loop with line = (split-sequence line #\: #\; #\, #\ )
        with id = (parse-integer (second line))
        for (count color) on (cddr line) by #'cddr
        if (string= color "red") maximize (parse-integer count) into red
        else if (string= color "green") maximize (parse-integer count) into green
        else if (string= color "blue") maximize (parse-integer count) into blue
        finally (return (list :id id :red red :green green :blue blue))))

(let ((input (parse-input :pre #'maximums)))
  (format t "~D~&" (reduce #'+ (mapcar (lambda (line)
                                         (if (and (<= (getf line :red) 12)
                                                  (<= (getf line :green) 13)
                                                  (<= (getf line :blue) 14))
                                             (getf line :id)
                                             0))
                                       input)))
  (format t "~D~&" (reduce #'+ (mapcar (lambda (line)
                                         (* (getf line :red)
                                            (getf line :green)
                                            (getf line :blue)))
                                       input))))
