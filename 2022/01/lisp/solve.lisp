(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(let ((elves (loop for elf = (parse-input :until "" :pre #'parse-integer)
                   while elf
                   collect (reduce #'+ elf))))
  (format t "~D~&" (reduce #'max elves))
  (format t "~D~&" (reduce #'+ (subseq (sort elves #'>) 0 3)))
  )
