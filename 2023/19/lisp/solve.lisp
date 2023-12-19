(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defstruct part
  (x 0 :type fixnum)
  (m 0 :type fixnum)
  (a 0 :type fixnum)
  (s 0 :type fixnum))

(defun part-sum (part)
  (+ (part-x part) (part-m part) (part-a part) (part-s part)))

(defun range-valid (start end)
  (and (> (part-x end) (part-x start))
       (> (part-m end) (part-m start))
       (> (part-a end) (part-a start))
       (> (part-s end) (part-s start))))

(defun to-symbol (string)
  (read-from-string (concatenate 'string "aoc-" string)))

(defun aoc-a (start &optional end)
  (if end
      (* (- (part-x end) (part-x start))
         (- (part-m end) (part-m start))
         (- (part-a end) (part-a start))
         (- (part-s end) (part-s start)))
      nil))

(defun aoc-r (start &optional end)
  (declare (ignore start))
  (if end 0 t))

(defun parse-workflow (line)
  (let ((split (split-sequence line #\{ #\} #\, #\:)))
       (cons (to-symbol (first split))
             (loop for (a b) on (rest split) by #'cddr
                   if b collect (read-from-string (subseq a 1 2)) ; < or >
                   and collect (read-from-string                  ; x, m, a, or s
                                 (concatenate 'string "part-" (subseq a 0 1)))
                   and collect (parse-integer a :start 2)         ; cmp value
                   and collect b                                  ; next workflow
                   else collect a))))

(defun setup-workflows (workflows)
  (loop for (wf-name . wf) in workflows
        do (eval `(defun ,wf-name (start &optional end)
                    (if end
                        (if (range-valid start end)
                            (+ ,@(loop for (cmp var val fun) on wf by #'cddddr
                                       if var
                                       collect `(let ((s (copy-structure start))
                                                      (e (copy-structure end)))
                                                  (setf (,var ,(ecase cmp
                                                                 (> 's) (< 'start)))
                                                        ,(+ val (ecase cmp (> 1) (< 0))))
                                                  (setf (,var ,(ecase cmp
                                                                 (> 'end) (< 'e)))
                                                        ,(+ val (ecase cmp (> 1) (< 0))))
                                                  (,(to-symbol fun) s e))
                                       else collect `(,(to-symbol cmp) start end)))
                            0)
                        (cond ,@(loop for (cmp var val fun) on wf by #'cddddr
                                      if var collect `((,cmp (,var start) ,val)
                                                       (,(to-symbol fun) start))
                                      else collect `(t (,(to-symbol cmp) start)))))))))

(defun parse-part (line)
  (apply #'make-part (loop for (c v) on (split-sequence line #\{ #\} #\, #\=) by #'cddr
                           collect (read-from-string (concatenate 'string ":" c))
                           collect (parse-integer v))))

(setup-workflows (parse-input :pre #'parse-workflow :until ""))
(let* ((parts (parse-input :pre #'parse-part)))
  (format t "~D~&" (reduce #'+ (mapcar #'part-sum (remove-if (to-symbol "in") parts))))
  (format t "~D~&" (funcall (to-symbol "in")
                            (make-part :x 1 :m 1 :a 1 :s 1)
                            (make-part :x 4001 :m 4001 :a 4001 :s 4001))))
