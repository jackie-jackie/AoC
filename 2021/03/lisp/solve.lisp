(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun bit-modes (seq &optional (invert nil))
  (map 'vector
       (lambda (x) (if (not (eql invert (>= x (/ (length seq) 2)))) 1 0))
       (reduce (lambda (a b) (map 'vector #'+ a b))
               seq))
  )

(defun bits-to-int (bit-vec)
  (parse-integer (coerce (map 'vector #'digit-char bit-vec) 'string) :radix 2)
  )

(let ((input (parse-input :pre (lambda (line) (map 'vector #'digit-char-p line)))))
  (format t "~D~&" (* (bits-to-int (bit-modes input))
                      (bits-to-int (bit-modes input t))))
  (format t "~D~&" (loop for i from 0 below (length (car input))
                         with workset = input
                         with ~workset = input
                         if (> (length workset) 1)
                         do (setf workset (remove (aref (bit-modes workset) i)
                                                  workset
                                                  :key (lambda (x) (aref x i))))
                         if (> (length ~workset) 1)
                         do (setf ~workset (remove (aref (bit-modes ~workset t) i)
                                                  ~workset
                                                  :key (lambda (x) (aref x i))))
                         finally (return (* (bits-to-int (car workset))
                                            (bits-to-int (car ~workset))))))
  )
