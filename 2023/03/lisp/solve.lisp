(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))
(load (merge-pathnames "../../../common/lisp/grid.lisp" *load-truename*))

(defun engine-p (elem)
  (and (typep elem 'character) (char/= #\. elem)))

(defun unique-part-numbers (list)
  (mapcar #'car (remove-duplicates (remove-if-not #'numberp list :key #'car))))

(defun ensure-number (char/number)
  (if (numberp char/number) char/number (digit-char-p char/number)))

(let* ((input (make-grid (parse-input :pre (lambda (line) (coerce line 'list)))))
       (part-numbers (make-array (array-dimensions input)))
       (gear-ratios (make-array (array-dimensions input))))
  (loop for row from 0 below (array-dimension input 0)
        ;; combine digits into numbers
        do (loop for col from 0 below (array-dimension input 1)
                 for index from (* row (array-dimension input 1))
                 for digit = (ensure-number (aref input row col))
                 for next-digit = (if (array-in-bounds-p input row (1+ col))
                                      (ensure-number (aref input row (1+ col)))
                                      nil)
                 if digit
                 do (setf (aref input row col) digit)
                 if (and digit next-digit)
                 do (setf (aref input row (1+ col)) (+ (* 10 digit) next-digit))
                 ;; run always
                 do (setf (aref input row col) (cons (aref input row col) index)))
        ;; spread numbers
        do (loop for col from (1- (array-dimension input 1)) downto 1
                 for number = (aref input row col)
                 if (and (numberp (car number))
                         (numberp (car (aref input row (1- col)))))
                 do (setf (aref input row (1- col)) number)))
  ;; part 1
  (grid-stencil9 (lambda (self &rest neighbors)
                   (if (or (not (numberp (car self)))
                           (find-if #'engine-p neighbors :key #'car))
                       self
                       '(0 . 0)))
                 input
                 :target-grid part-numbers)
  ;; part 2
  (grid-stencil9 (lambda (self &rest neighbors)
                   (let ((parts (unique-part-numbers neighbors)))
                     (if (and (equal #\* (car self)) (= 2 (length parts)))
                         (* (first parts) (second parts))
                         0)))
                 input
                 :target-grid gear-ratios)
  (format t "~D~&" (reduce #'+ (unique-part-numbers (grid-list part-numbers))))
  (format t "~D~&" (reduce #'+ (grid-list gear-ratios))))
