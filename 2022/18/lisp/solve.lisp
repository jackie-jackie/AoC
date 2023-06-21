(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defmacro axis (a &optional point)
  (if point
      `(nth (mod ,a 3) ,point)
      `(lambda (point)
        (nth (mod ,a 3) point)
        ))
  )

(defun surface (blocks axis)
  (* 2 (loop for (a b) on (stable-sort (stable-sort (sort (copy-seq blocks)
                                                          #'<
                                                          :key (axis axis))
                                                    #'<
                                                    :key (axis (1+ axis)))
                                       #'<
                                       :key (axis (1- axis)))
             count (not (and b
                             (equal (axis (1+ axis) a) (axis (1+ axis) b))
                             (equal (axis (1- axis) a) (axis (1- axis) b))
                             (<= (- (axis axis b) (axis axis a)) 1)))
             ))
  )

(let ((blocks (parse-input :pre (lambda (line)
                                  (mapcar #'parse-integer
                                          (split-sequence line #\,))
                                  ))))
  (format t "~D~&" (loop for x from 0 to 2
                         sum (surface blocks x)))
  (format t "~D~&" nil)
  )
