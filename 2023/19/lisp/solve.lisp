(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(declaim (optimize (speed 3) (safety 0)))

(defstruct part
  (x 0 :type fixnum)
  (m 0 :type fixnum)
  (a 0 :type fixnum)
  (s 0 :type fixnum))

(defun part-sum (part)
  (+ (part-x part) (part-m part) (part-a part) (part-s part)))

(defun aoc-a (part)
  (declare (ignore part))
  t)

(defun aoc-r (part)
  (declare (ignore part))
  nil)

(defmacro namespace (string)
  `(concatenate 'string "aoc-" ,string))

(defmacro parse-workflow ()
  `(lambda (line)
     (let ((split (split-sequence line #\{ #\} #\, #\:)))
       (cons (read-from-string (namespace (first split)))
             (loop for (a b) on (rest split) by #'cddr
                   if b collect (read-from-string (subseq a 1 2))
                   and collect (read-from-string (concatenate 'string "part-" (subseq a 0 1)))
                   and collect (parse-integer a :start 2)
                   and collect (read-from-string (namespace b))
                   else collect (read-from-string (namespace a)))))))

(defmacro setup-workflows ()
  (let ((workflows (parse-input :pre (parse-workflow) :until "")))
    `(progn
       (defvar *workflows* (quote ,workflows))
       ,@(loop for workflow in workflows
               collect `(defun ,(first workflow) (part)
                          (declare (type part part))
                          (cond ,@(loop for (cmp var val fun) on (rest workflow) by #'cddddr
                                        if var collect `((,cmp (,var part) ,val)
                                                         (,fun part))
                                        else collect `(t (,cmp part)))))))))

(setup-workflows)

(defun parse-part (line)
  (apply #'make-part (loop for (c v) on (split-sequence line #\{ #\} #\, #\=) by #'cddr
                           collect (read-from-string (concatenate 'string ":" c))
                           collect (parse-integer v))))

(let* ((parts (parse-input :pre #'parse-part)))
  (format t "~D~&" (reduce #'+ (mapcar #'part-sum (remove-if-not #'aoc-in parts))))
  (quit) ; finishes in about 240 days; but trivially parallelizable, so just get 40000 cores and you're fine
  (format t "~D~&" (loop for x from 1 to 4000
                         sum
                         (loop for m from 1 to 4000
                               sum
                               (loop for a from 1 to 4000
                                     sum
                                     (loop for s from 1 to 4000
                                           count
                                           (aoc-in (make-part :x x :m m :a a :s s))))))))
