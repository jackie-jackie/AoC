(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun engine-p (elem)
  (and (typep elem 'character) (char/= #\. elem)))

(defun sum-numbers (grid)
  (realpart (reduce #'+
                    (remove-duplicates (remove-if-not #'numberp
                                                      (grid-list grid :line-breaks t))))))

(defun ensure-number (char/number)
  (if (numberp char/number) char/number (digit-char-p char/number)))

(let* ((input (make-grid (parse-input :pre (lambda (line) (coerce line 'list)))))
       (part-numbers (make-array (array-dimensions input)))
       (gear-ratios (make-array (array-dimensions input))))
  (loop for row from 0 below (array-dimension input 0)
        ;; combine digits into numbers
        do (loop for col from 0 below (array-dimension input 1)
                 for digit = (ensure-number (aref input row col))
                 for next-digit = (if (array-in-bounds-p input row (1+ col))
                                      (ensure-number (aref input row (1+ col)))
                                      nil)
                 if digit
                 do (setf (aref input row col) digit)
                 if (and digit next-digit)
                 do (setf (aref input row (1+ col)) (+ (* 10 digit) next-digit)))
        ;; do some complex number abuse
        do (loop for col from 0 below (array-dimension input 1)
                 for index from (* row (array-dimension input 1))
                 for number = (aref input row col)
                 if (and (numberp number) (= 0 (imagpart number)))
                 do (setf (aref input row col) (+ number (complex 0 index))))
        ;; spread numbers
        do (loop for col from (1- (array-dimension input 1)) downto 1
                 for number = (aref input row col)
                 if (and (numberp number) (numberp (aref input row (1- col))))
                 do (setf (aref input row (1- col)) number)))
  ;; part 1
  (grid-stencil9 (lambda (self &rest neighbors)
                   (if (or (not (numberp self)) (find-if #'engine-p neighbors))
                       self
                       0))
                 input
                 :target-grid part-numbers)
  ;; part 2
  (grid-stencil9 (lambda (self &rest neighbors)
                   (let ((parts (remove-duplicates (remove-if-not #'numberp neighbors))))
                     (if (and (equal #\* self) (= 2 (length parts)))
                         (* (realpart (first parts)) (realpart (second parts)))
                         0)))
                 input
                 :target-grid gear-ratios)
  (format t "~D~&" (sum-numbers part-numbers))
  (format t "~D~&" (sum-numbers gear-ratios)))
