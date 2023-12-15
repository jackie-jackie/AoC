(load (merge-pathnames "../../../common/lisp/util.lisp" *load-truename*))

(defun hash (string)
  (logand 255 (reduce (lambda (a b) (* 17 (+ a (char-code b)))) string :initial-value 0)))

(defun hashmap-insert (hashmap key value)
  (let ((slot (assoc key (aref hashmap (hash key)) :test #'string=)))
    (if slot (rplacd slot value) (push (cons key value) (aref hashmap (hash key))))))

(defun hashmap-delete (hashmap key)
  (setf (aref hashmap (hash key))
        (delete key (aref hashmap (hash key)) :test #'string= :key #'car)))

(defun focusing-power (hashmap)
  (loop for i from 0 below (array-total-size hashmap)
        sum (loop for (nil . value) in (reverse (aref hashmap i))
                  for slot from 1
                  sum (* (1+ i) slot value))))

(defun parse-instruction (instruction)
  (let ((split (or (position #\= instruction) (position #\- instruction))))
    (list (char instruction split)
          (subseq instruction 0 split)
          (parse-integer (subseq instruction (1+ split)) :junk-allowed t))))

(let ((input (flatten (parse-input :pre (lambda (line) (split-sequence line #\,))))))
  (format t "~D~&" (reduce #'+ (mapcar #'hash input)))
  (format t "~D~&" (loop with hashmap = (make-array 256 :initial-element '())
                         for instruction in input
                         for (op key value) = (parse-instruction instruction)
                         if (char= op #\=) do (hashmap-insert hashmap key value)
                         else if (char= op #\-) do (hashmap-delete hashmap key)
                         finally (return (focusing-power hashmap)))))
