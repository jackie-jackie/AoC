(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun monkey-int (name)
  (loop for c across name
        for sum = (char- c #\a) then (+ (char- c #\a) (* 26 sum))
        finally (return sum)))

(defun parse-line (line)
  (loop with split = (split-sequence line #\  #\:)
        for s in split
        for i from 0
        if (zerop i) collect (monkey-int s)
        else if (= (length split) 2) collect (parse-integer s)
        else if (= i 2) collect (case (char s 0) (#\+ #'+) (#\- #'-) (#\* #'*) (#\/ #'/))
        else collect (monkey-int s)))

(defun yells (monkey monkeys)
  (let ((formula (assoc monkey monkeys)))
    (if (= 2 (length formula))
        (cadr formula)
        (funcall (caddr formula)
                 (yells (cadr formula) monkeys)
                 (yells (cadddr formula) monkeys)))))

(defun solve (x y)
  (cond ((complexp x) (/ (- y (realpart x)) (imagpart x)))
        ((complexp y) (/ (- x (realpart x)) (imagpart x)))))

(let ((monkeys (parse-input :pre #'parse-line))
      (root (monkey-int "root"))
      (humn (monkey-int "humn")))
  (format t "~D~&" (yells root monkeys))
  (setf (caddr (assoc root monkeys)) #'solve
        (cadr (assoc humn monkeys)) #C(0 1) )
  (format t "~D~&" (yells root monkeys)))
