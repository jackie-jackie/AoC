(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun contains (bags color)
  (loop for (bag . count) in (cdr (assoc color bags :test #'string=))
        sum (* count (1+ (contains bags bag)))))

(let ((bags (parse-input :pre (lambda (line)
                                (loop for (a b c d) on (split-space line)
                                      by #'cddddr
                                      for i from 0
                                      if (= i 0)
                                        collect (concatenate 'string a " " b)
                                      else if d
                                        collect (cons (concatenate 'string b " " c)
                                                      (parse-integer a)))))))
  (format t "~D~&" (loop with colors = '()     ; bags that can contain a shiny gold bag
                         with remainder = bags ; rules for bags not yet considered
                         with stop = nil       ; stop when no new bags are found
                         until stop
                         do (loop for (outer . inner) in remainder
                                  if (some (lambda (b)
                                             (find (car b)
                                                   (cons "shiny gold" colors)
                                                   :test #'string=))
                                           inner)
                                    collect outer into good
                                  else
                                    collect (cons outer inner) into bad
                                  finally
                                    (setf stop (not good)
                                          colors (append colors good)
                                          remainder bad))
                         finally (return (length colors))))
  (format t "~D~&" (contains bags "shiny gold")))
