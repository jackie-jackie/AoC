(load "../../../common/lisp/util.lisp")

(let ((elves (loop for elf = (parse-input :until ""
                                            :pre parse-integer)
                   until (not elf)
                   collect (reduce #'+ elf))))
  (format t "~D~%" (reduce #'max elves))
  (format t "~D~%" (reduce #'+ (subseq (sort elves #'>) 0 3)))
  )
