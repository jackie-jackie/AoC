(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun win-count (card)
  (length (intersection (second card) (third card))))

(defun win-value (card)
  (floor (expt 2 (1- (win-count card)))))

(defun parse-line (line)
  (cons 1 (mapcar (lambda (numbers) (mapcar #'parse-integer numbers))
                  (mapcar #'split-space (rest (split-sequence line #\: #\|))))))

(let ((cards (parse-input :pre #'parse-line)))
  (format t "~D~&" (reduce #'+ (mapcar #'win-value cards)))
  (format t "~D~&" (loop for (card . rest) on cards
                         sum (first card)
                         do (loop for next-card in rest
                                  repeat (win-count card)
                                  do (incf (first next-card) (first card))))))
